#!/usr/bin/env python3

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

'''
This is a minimalistic mirth interpreter. It is not designed to be
correct or complete -- it is designed to be a small first step to
bootstrapping the compiler. Never use this without copious tests.

USAGE:

    python3 mirth.py                    Start REPL.
    python3 mirth.py --doctest          Run doc tests.
    python3 mirth.py PATH [ARGS...]     Run mirth script with args.

FLAGS:
    --no-prelude    Don't auto-import prelude.
'''

import sys
import random
import os
import os.path
random.seed('mirth bootstrap')

def main():
    import sys

    flags = {
        'no-prelude': False,
        'typecheck': False,
        'testonly': False
    }

    def check_flag(p):
        try:
            i = sys.argv.index('--' + p)
            del sys.argv[i]
            flags[p] = True
        except ValueError:
            flags[p] = False

    check_flag('no-prelude')
    check_flag('typecheck')
    check_flag('testonly')

    if len(sys.argv) == 1:
        repl(flags)
    elif sys.argv[1:] == ['--doctest']:
        import doctest
        doctest.testmod()
    else:
        interpret(sys.argv[1], sys.argv[2:], flags)

def load_prelude():
    p = os.path.dirname(os.path.realpath(__file__))
    p = os.path.join(p, 'prelude.mth')
    with open(p) as fp:
        ps, ds = parse(fp)

    m = module()
    for d in ds:
        if not isinstance(d, assertion): # don't check assertions for import
            d.decl(m)

    return m

def interpret(path, args, flags):
    try:
        with open(path) as fp:
            preamble, decls = parse(fp)
        if preamble:
            raise SyntaxError("%s: import/export statements in single file mode." % path)

        m = load_prelude() if not flags['no-prelude'] else module()
        for d in decls:
            d.decl(m)

        if not flags['typecheck']:
            m.check_assertions()

        if 'main' in m.word_defs and not flags['typecheck'] and not flags['testonly']:
            (ps,dom,cod) = m.word_sigs['main']
            exp_dom = tpack(None, [tlist(tstr)] if len(dom.args) > 0 else [])
            exp_cod = tpack(None, [tint] if len(cod.args) > 0 else [], cod.tags)
            if ps != [] or dom != exp_dom or cod != exp_cod:
                raise TypeError("%s: Unexpected type signature for main. Should be\n  main : List(Str) -- Int +t1 +t2 ... +tn" % path)
            e = env()
            e.push(args)
            e.copush(m.word_defs['main'])
            e.run(timeout=10000000000000)
            sys.exit(e.pop())
    except IsADirectoryError as e:
        run_package(path, args, flags)
    except TypeError as e:
        print('TypeError:', e, file=sys.stderr)
        sys.exit(1)
    except SyntaxError as e:
        print('SyntaxError:', e, file=sys.stderr)
        sys.exit(1)

def repl(flags):
    # REPL banner
    print()
    print('Mirth Bootstrap Interpreter v0.0.0: CAVEAT USOR.')
    print()
    print('Thank you to our patrons on https://patreon.com/mirth_lang')
    print('And a special thanks to Benjohn, Joseph Victor, and spacekitteh')
    print()

    m = load_prelude() if not flags['no-prelude'] else module()
    e = env()
    l = word_elaborator(m, tpack())

    try:
        keep_going = True
        while keep_going:
            code = input(">>> ")
            if code.strip() == 'bye': break
            try:
                preamble, decls = parse(code)
                for decl in decls:
                    if isinstance(decl, expr):
                        f = decl.elab(l)
                        f(e)
                        e.run(timeout=1000000)
                    elif isinstance(decl, assertion):
                        decl.decl(m)
                        m.check_assertion(m.assertions[-1])
                    else:
                        decl.decl(m)
                e.show_stack()
            except SyntaxError as err:
                print(err)
            except TypeError as err:
                print(err)
            except NameError as err:
                print(err)
            except ValueError as err:
                print(err)

    except EOFError:
        print('bye')
    except KeyboardInterrupt:
        print()

##############################################################################
############################ IMPORT / EXPORT #################################
##############################################################################

# A package is a collection of Mirth modules, organized as a directory tree.
# The directory structure is not important since we pick up all Mirth files
# (i.e. all files with the .mth extension) recursively within the specified
# directory. The Mirth files in a package can use export / import to access
# each other's definitions.
#
# For example, if the file a.mth has the statement at the top:
#
#     export useful-interface
#       save-the-world : --
#     end
#
# And b.mth has the statement:
#
#     import useful-interface
#
# Then b.mth has access to the definition `save-the-world`. Notice that
# there is no relation between the import/export interface name and the
# file names. You can export multiple interfaces per file, and the same
# interface name can be exported in multiple files (it will be combined).
#
# An analogy with C may be useful. Each module body represents a .c file,
# whereas each interface represents a .h file, compiled from the export
# declarations in all the module preambles. The imports represent
# `#include` statements against those .h files.
#
# There are no inter-package imports/exports, at least, not in bootstrap.
# You can basically do this by including one package inside another,
# though that's obviously not ideal in the long term.

def get_shortname(pkg, modpath):
    return modpath[len(pkg)+1:]

def error(modpath, lineno, msg):
    p = os.path.relpath(modpath)
    if lineno is None:
        lineno = 1
    print('%s:%d:' % (p, lineno), msg, file=sys.stderr)
    sys.exit(1)

def handle_package_error(pkg, modpath, f):
    shortname = get_shortname(pkg, modpath)
    try:
        return f()
    except NameError as e:
        error(modpath, None, 'NameError: ' + str(e))
    except ValueError as e:
        error(modpath, None, 'ValueError: ' + str(e))
    except TypeError as e:
        error(modpath, None, 'TypeError: ' + str(e))
    except SyntaxError as e:
        error(modpath, None, 'SyntaxError: ' + str(e))

def run_package(pkg, args, flags):
    herr = lambda m,f: handle_package_error(pkg,m,f)
    modpaths = list_modules(pkg)
    modpreambles = {}
    modbodies = {}
    mods = {}
    interfaces = {}
    interfaces_orig = {}
    interfaces_defs = {}

    # TODO prevent two data defs with the same name in the same package
    #  -- a better solution would detect the actual problem of multiple
    #     independent type definitions colliding downstream, but this is
    #     the bootstrap and it's better to be incomplete than incorrect.

    for modpath in modpaths:
        with open(modpath) as fp:
            preamble, body = herr(modpath, lambda: parse(fp))
        modpreambles[modpath] = preamble
        modbodies[modpath] = body
        mods[modpath] = load_prelude() if not flags['no-prelude'] else module()
            # TODO cache the prelude instead of loading it repeatedly

        for pdecl in preamble:
            if pdecl[0] == 'export':
                iline, iname, ibody = pdecl[1:]
                if iname in interfaces:
                    omodpath, oline = interfaces_orig[iname]
                    error(modpath, iline,
                        "Duplicate interface %s with %s line %d."
                        % (iname, os.path.relpath(omodpath), oline)
                    )
                interfaces[iname] = ibody
                interfaces_orig[iname] = modpath, iline
                interfaces_defs[iname] = {}

    def mk_import_interface_fn (iname, dname):
        def f (*args):
            return interfaces_defs[iname][dname] (*args)
        return f

    for modpath in modpaths:
        m = mods[modpath]
        for pdecl in modpreambles[modpath]:
            if pdecl[0] == 'export':
                for decl in pdecl[3]:
                    herr(modpath, lambda: decl.decl(m))
            elif pdecl[0] == 'import':
                iline, iname = pdecl[1:]
                if iname not in interfaces:
                    error(modpath, iline, "Interface %s not declared in package." % iname)
                omodpath, oline = interfaces_orig[iname]
                for decl in interfaces[iname]:
                    herr(omodpath, lambda: decl.decl(m))
                    if isinstance(decl, word_sig):
                        dname = decl.name.code
                        m.word_defs[dname] = mk_import_interface_fn (iname, dname)

            else:
               error(modpath, None, "Unexpected preamble decl form: %r" % decl)

        for decl in modbodies[modpath]:
            herr(modpath, lambda: decl.decl(m))

        for pdecl in modpreambles[modpath]:
            if pdecl[0] == 'export':
                iname = pdecl[2]
                for decl in pdecl[3]:
                    if isinstance(decl, word_sig):
                        dname = decl.name.code
                        dline = decl.name.lineno
                        if dname not in m.word_defs:
                            error(modpath, dline, "Missing definition for exported word %s" % dname)
                        interfaces_defs[iname][dname] = m.word_defs[dname]

    if not flags['typecheck']:
        for modpath in modpaths:
            m = mods[modpath]
            herr(modpath, lambda: m.check_assertions())

    mainpath = os.path.join(pkg, 'main.mth')
    if mainpath in modpaths and not flags['typecheck'] and not flags['testonly']:
        m = mods[mainpath]
        if 'main' in m.word_defs:
            (ps,dom,cod) = m.word_sigs['main']
            exp_dom = tpack(None, [tlist(tstr)] if len(dom.args) > 0 else [])
            exp_cod = tpack(None, [tint] if len(cod.args) > 0 else [], cod.tags)
            if ps != [] or dom != exp_dom or cod != exp_cod:
                error(mainpath, None, "Unexpected type signature for main. Should be\n  main : List(Str) -- Int +t1 +t2 ... +tn")
            e = env()
            e.push(args)
            e.copush(m.word_defs['main'])
            e.run(timeout=10000000000000)
            sys.exit(e.pop())
        else:
            error(mainpath, None, "Expected definition of main.")

def list_modules(pkg):
    '''List all modules inside a given package.'''
    ms = []
    for root, dirs, files in os.walk(pkg):
        ms.extend(os.path.join(root, fp) for fp in files if fp[-4:] == ".mth")
    return ms

##############################################################################
################################# LEXING #####################################
##############################################################################

import re
lexer_rules = [ re.compile(r) for r in [
    r'\(',
    r'\)',
    r',',
    r':',
    r'#.*',
    r'\|\|\|.*',
    r'\"([^\\\"\n]|\\[ntr\"\\])*\"',
    r'[^ \t\n\r":\(\),]+'
]]

def tokenize (code):
    r'''tokenize (file or string) -> token_generator

    Lex a mirth codebase.

    >>> list(tokenize('foo bar'))
    [token('foo', 1), token('bar', 1), token('\n', 1)]
    >>> list(tokenize('foo:bar'))
    [token('foo', 1), token(':', 1), token('bar', 1), token('\n', 1)]
    >>> list(tokenize('foo"bar baz"'))
    [token('foo', 1), token('"bar baz"', 1), token('\n', 1)]
    '''

    if isinstance(code, str):
        lines = code.split('\n')
    else:
        lines = list(code)

    for i, line in enumerate(lines):
        emitted = False
        line = line.lstrip()
        while line:
            for rule in lexer_rules:
                m = rule.match(line)
                if m:
                    tok = m.group(0)
                    line = line[len(tok):].lstrip()
                    if tok[0] != '#' and tok[:3] != '|||':
                        emitted = True
                        yield token(code=tok, lineno=i+1)
                    break
            else:
                raise SyntaxError("Unknown token on line %d: %r" % (i+1, line))
        if emitted:
            yield token(code='\n', lineno=i+1)

# tokens with special meaning
reserved = {'\n', '(', ')', ',', ':', '--', '=', ':=', '==', 'type', 'data', 'end', 'import', 'export'}

class token(object):
    def __init__(self, code, lineno):
        self.code = code
        self.lineno = lineno

    def __repr__(self):
        return 'token(%r, %d)' % (self.code, self.lineno)

    def __str__(self):
        return self.code

    def is_newline (self): return self.code == '\n'
    def is_lparen  (self): return self.code == '('
    def is_rparen  (self): return self.code == ')'
    def is_comma   (self): return self.code == ','
    def is_colon   (self): return self.code == ':'
    def is_dash2   (self): return self.code == '--'
    def is_equal   (self): return self.code == '='
    def is_coloneq (self): return self.code == ':='
    def is_equal2  (self): return self.code == '=='
    def is_type    (self): return self.code == 'type'
    def is_data    (self): return self.code == 'data'
    def is_end     (self): return self.code == 'end'
    def is_import  (self): return self.code == 'import'
    def is_export  (self): return self.code == 'export'

    def is_int(self):
        try:
            int(self.code, 0)
        except:
            return False
        return True

    def is_str(self):
        return self.code[0] == '"'

    def is_name(self):
        return not (self.code in reserved
                 or self.is_str()
                 or self.is_int())

    def to_str(self):
        src = self.code[1:-1]
        cs = []
        while src:
            if src[0] == '\\':
                if len(src) < 2:
                    raise ValueError("Invalid string token: %r" % self.code)
                elif src[1] == 'n':
                    cs.append('\n')
                elif src[1] == 't':
                    cs.append('\t')
                elif src[1] == 'r':
                    cs.append('\r')
                elif src[1] == '\\':
                    cs.append('\\')
                elif src[1] == '\"':
                    cs.append('\"')
                else:
                    raise ValueError("Invalid string token, unknown escape combination: %r" % src[0:1])
                src = src[2:]
            else:
                cs.append(src[0])
                src = src[1:]
        return ''.join(cs)

##############################################################################
################################# PARSING ####################################
##############################################################################

def parsetoks(tokens):
    tokens = list(tokens)

    # combinators

    def memo(p):
        m = {}
        def f(i):
            if i not in m:
                m[i] = p(i)
            return m[i]
        return f

    def seq(*ps):
        def f(i):
            vs = []
            for p in ps:
                r = p(i)
                if r is None:
                    return None
                v, i = r
                vs.append(v)
            return tuple(vs), i
        return f

    def alt(*ps):
        def f(i):
            for p in ps:
                r = p(i)
                if r is not None:
                    return r
        return f

    def star(p):
        def f(i):
            vs = []
            r = p(i)
            while r is not None:
                v, j = r
                if j <= i:
                    if i < len(tokens):
                        raise SyntaxError("infinite loop detected at line %s" % tokens[i].lineno)
                    else:
                        raise SyntaxError("infinite loop detected at EOF")
                i = j
                vs.append(v)
                r = p(i)
            return vs, i
        return f

    def starsep(sep, p):
        return fmapseq(
            (lambda a,b: [a] + b),
            p,
            star(fmapseq((lambda a,b: b), sep, p))
        )

    def fmap(g, p):
        def f(i):
            r = p(i)
            if r is not None:
                v, i = r
                return g(v), i
        return f

    def fmapseq(g, *ps):
        return fmap(lambda vs: g(*vs), seq(*ps))

    def parens(p):
        return fmapseq((lambda a,b,c: b),
            test(token.is_lparen),
            p,
            test(token.is_rparen)
        )

    def pure(f):
        return lambda i: (f(), i)

    def test(t):
        def f(i):
            if i < len(tokens) and t(tokens[i]):
                return tokens[i], i+1
        return f

    # parsers

    p_int  = memo(fmap(intlit, test(token.is_int)))
    p_str  = memo(fmap(strlit, test(token.is_str)))
    p_name = memo(test(token.is_name))
    p_line = test(token.is_newline)
    p_comma = test(token.is_comma)

    @memo
    def p_expr_ignore_line(i):
        return fmap(expr, star(alt(p_atom, p_line))) (i)

    p_args = memo(alt(
        parens(starsep(p_comma, p_expr_ignore_line)),
        pure(lambda: []),
        ))
    p_word = memo(fmapseq(word, p_name, p_args))
    p_atom = memo(alt(p_str, p_int, p_word))
    p_expr = memo(fmap(expr, star(p_atom)))

    p_word_sig_param = fmapseq(lambda _0,a,_,b,c: (a,b,c),
        star(p_line),
        p_name,
        test(token.is_colon),
        alt(
            fmapseq(lambda a,b: a,
                p_expr_ignore_line,
                test(token.is_dash2)
            ),
            pure(lambda: expr([]))
        ),
        p_expr_ignore_line
    )

    p_word_sig_params = alt(
        parens(starsep(p_comma, p_word_sig_param)),
        pure(lambda: []),
    )

    p_word_sig = fmapseq(lambda a,p,_1,_2,_3,b,c: word_sig(a,p,b,c),
        p_name,
        p_word_sig_params,
        star(p_line),
        test(token.is_colon),
        star(p_line),
        alt(
            fmapseq(lambda a,b: a,
                p_expr,
                test(token.is_dash2),
            ),
            pure(lambda: expr([]))
        ),
        p_expr
    )

    p_word_def_params = alt(
        parens(starsep(
            seq(star(p_line), p_comma, star(p_line)),
            fmapseq(lambda a,b,c: b, star(p_line), p_name, star(p_line))
        )),
        pure(lambda: []),
    )

    p_word_def = fmapseq(lambda a,p,_1,_2,_3,b: word_def(a,p,b),
        p_name,
        p_word_def_params,
        star(p_line),
        test(token.is_equal),
        star(p_line),
        p_expr,
    )

    p_assertion = fmapseq(lambda a,_1,c,_2,b: assertion(c.lineno, a,b),
        p_expr,
        star(p_line),
        test(token.is_equal2),
        star(p_line),
        p_expr,
    )

    p_type_sig = fmapseq(lambda a,b,c: type_sig(a.lineno, b,c),
        test(token.is_type),
        p_name,
        p_word_def_params,
    )

    p_data_def = fmapseq(lambda a,b,c,d,e,f: data_def(a.lineno, b, c, e),
        test(token.is_data),
        p_name,
        p_word_def_params,
        p_line,
        star(fmapseq(lambda a,b: a, p_word_sig, p_line)),
        test(token.is_end)
    )

    p_export_decl = fmapseq(lambda a,b: a,
        alt(
            p_word_sig,
            p_type_sig
        ),
        p_line,
    )

    p_export = fmapseq(lambda a,b,c,d,e: ('export', a.lineno, b.code, d),
        test(token.is_export),
        test(token.is_name),
        p_line,
        star(p_export_decl),
        test(token.is_end),
    )

    p_import = fmapseq(lambda a,b: ('import', a.lineno, b.code),
        test(token.is_import),
        test(token.is_name),
    )

    p_preamble_decl = fmapseq(lambda a,b: a,
        alt(
            p_export,
            p_import,
        ),
        p_line
    )

    p_body_decl = fmapseq(lambda a,b: a,
        alt(
            p_word_sig,
            p_word_def,
            p_assertion,
            p_type_sig,
            p_data_def,
            p_expr
        ),
        p_line
    )

    p_preamble = star(p_preamble_decl)
    p_body = star(p_body_decl)
    p_file = seq(p_preamble, p_body)

    r = p_file(0)
    if not r:
        raise SyntaxError("parse error at unknown line")
    v,i = r
    if i < len(tokens):
        raise SyntaxError("parse error at line %d" % tokens[i].lineno)
    return v

def parse(code):
    '''Parse code or lines of code.

    >>> [str(x) for x in parse('foo bar')[1]]
    ['foo bar']
    >>> [str(x) for x in parse('10 20 +')[1]]
    ['10 20 +']
    >>> parse('10')[1]
    [expr([intlit(token('10', 1))])]
    >>> parse('foo(bar)')[1]
    [expr([word(token('foo', 1), [expr([word(token('bar', 1), [])])])])]
    >>> parse('foo : bar')[1]
    [word_sig(token('foo', 1), [], expr([]), expr([word(token('bar', 1), [])]))]
    >>> parse('foo : bar -- baz')[1]
    [word_sig(token('foo', 1), [], expr([word(token('bar', 1), [])]), expr([word(token('baz', 1), [])]))]
    >>> parse('foo(f : bar) : baz')[1]
    [word_sig(token('foo', 1), [(token('f', 1), expr([]), expr([word(token('bar', 1), [])]))], expr([]), expr([word(token('baz', 1), [])]))]
    >>> parse('foo = bar')[1]
    [word_def(token('foo', 1), [], expr([word(token('bar', 1), [])]))]
    >>> parse('foo == bar')[1]
    [assertion(1, expr([word(token('foo', 1), [])]), expr([word(token('bar', 1), [])]))]
    '''
    return parsetoks(tokenize(code))

##############################################################################
################################# SYNTAX #####################################
##############################################################################

class intlit:
    def __init__(self, token):
        self.token = token
        self.value = int(token.code, 0)

    def __repr__(self):
        return 'intlit(%r)' % self.token

    def __str__(self):
        return str(self.value)

    def elab(self, env):
        return env.elab_push_int(self.value)

class strlit:
    def __init__(self, token):
        self.token = token
        self.value = token.to_str()

    def __repr__(self):
        return 'strlit(%r)' % self.token

    def __str__(self):
        return self.value

    def elab(self, env):
        return env.elab_push_str(self.value)

class word:
    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __repr__(self):
        return 'word(%r, %r)' % (self.name, self.args)

    def __str__(self):
        if self.args:
            return '%s(%s)' % (self.name,
                ', '.join(str(a) for a in self.args))
        else:
            return str(self.name)

    def elab(self, env):
        return env.elab_word(self.name.code, self.args)

class expr:
    def __init__(self, atoms):
        self.atoms = [p for p in atoms if not isinstance(p, token)]

    def __repr__(self):
        return 'expr(%r)' % self.atoms

    def __str__(self):
        return ' '.join(str(p) for p in self.atoms)

    def elab(self, env):
        return env.elab_expr(self.atoms)

    def decl(self, mod):
        return mod.decl_expr(self)

    def split_on(self, name):
        """
        >>> list(expr([]).split_on('->'))
        [expr([])]
        >>> list(expr([word(token('->', 0), [])]).split_on('->'))
        [expr([]), expr([])]
        """
        i = 0
        for j,atom in enumerate(self.atoms):
            if isinstance(atom, word) and atom.name.code == name and len(atom.args) == 0:
                yield expr(self.atoms[i:j])
                i = j+1
        yield expr(self.atoms[i:])

class word_sig:
    def __init__(self, name, params, dom, cod):
        self.name = name
        self.params = params
        self.dom = dom
        self.cod = cod

    def __repr__(self):
        return 'word_sig(%r, %r, %r, %r)' % (self.name, self.params, self.dom, self.cod)

    def decl(self, mod):
        try:
            return mod.decl_word_sig(self.name.code, self.params, self.dom, self.cod)
        except TypeError as e:
            raise TypeError("line %d: %s" % (self.name.lineno, e)) from e
        except SyntaxError as e:
            raise SyntaxError("line %d: %s" % (self.name.lineno, e)) from e

class word_def:
    def __init__(self, name, params, body):
        self.name   = name
        self.params = params
        self.body   = body

    def __repr__(self):
        return 'word_def(%r, %r, %r)' % (self.name, self.params, self.body)

    def decl(self, mod):
        try:
            return mod.decl_word_def(self.name.code, [p.code for p in self.params], self.body)
        except TypeError as e:
            raise TypeError("line %d: %s" % (self.name.lineno, e)) from e
        except SyntaxError as e:
            raise SyntaxError("line %d: %s" % (self.name.lineno, e)) from e

class assertion:
    def __init__(self, lineno, lhs, rhs):
        self.lineno = lineno
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self):
        return 'assertion(%r, %r, %r)' % (self.lineno, self.lhs, self.rhs)

    def decl(self, mod):
        try:
            return mod.decl_assertion(self.lineno, self.lhs, self.rhs)
        except TypeError as e:
            raise TypeError("line %d: %s" % (self.lineno, e)) from e
        except SyntaxError as e:
            raise SyntaxError("line %d: %s" % (self.lineno, e)) from e

class type_sig:
    def __init__(self, lineno, name, params):
        self.lineno = lineno
        self.name = name
        self.params = params

    def __repr__(self):
        return ("type_sig(%r, %r, %r)" %
            (self.lineno, self.name, self.params))

    def decl(self, mod):
        try:
            return mod.decl_type_sig(self.lineno, self.name.code, self.params)
        except TypeError as e:
            raise TypeError("line %d: %s" % (self.lineno, e)) from e
        except SyntaxError as e:
            raise SyntaxError("line %d: %s" % (self.lineno, e)) from e

class data_def:
    def __init__(self, lineno, name, params, wordsigs):
        self.lineno = lineno
        self.name = name
        self.params = params
        self.wordsigs = wordsigs

    def __repr__(self):
        return ('data_def(%r, %r, %r, %r)' %
            (self.lineno, self.name, self.params, self.wordsigs))

    def decl(self, mod):
        try:
            return mod.decl_data_def(self.lineno, self.name.code, self.params, self.wordsigs)
        except TypeError as e:
            raise TypeError("line %d: %s" % (self.lineno, e)) from e
        except SyntaxError as e:
            raise SyntaxError("line %d: %s" % (self.lineno, e)) from e

##############################################################################
########################### TYPES & UNIFICATION ##############################
##############################################################################

class tcon:
    def __init__(self, name, args=()):
        self.name = name
        self.args = list(args)

    def __repr__(self):
        if self.args:
            return 'tcon(%r, %r)' % (self.name, self.args)
        else:
            return 'tcon(%r)' % self.name

    def __str__(self):
        if self.args:
            return '%s(%s)' % (self.name, ', '.join(map(str, self.args)))
        else:
            return self.name

    def __eq__ (self, other):
        return self.name == other.name and self.args == other.args

    def rigidify(self):
        return tcon(self.name, [arg.rigidify() for arg in self.args])

    def freshen(self, prefix):
        return tcon(self.name, [arg.freshen(prefix) for arg in self.args])

    def has_var(self, name):
        return any(arg.has_var(name) for arg in self.args)

    def subst(self, sub):
        """
        >>> tcon('foo', [tvar('x')]).subst({'x': tvar('y')})
        tcon('foo', [tvar('y')])
        """
        return tcon(self.name, [arg.subst(sub) for arg in self.args])

    def unify(self, other, sub):
        return other.unify_tcon(self.name, self.args, sub)

    def unify_tcon(self, other_name, other_args, sub):
        if self.name == other_name and len(self.args) == len(other_args):
            uargs = []
            for (sarg, oarg) in zip(self.args, other_args):
                uarg = oarg.unify(sarg, sub)
                uargs.append(uarg)
            return tcon(self.name, [uarg.subst(sub) for uarg in uargs])
        else:
            raise TypeError("Failed to unify %s and %s." % (self, tcon(other_name, other_args)))

    def unify_tvar(self, other_name, sub):
        return tvar(other_name).unify_tcon(self.name, self.args, sub)

    def unify_tpack(self, other_rest, other_args, other_tags, sub):
        if other_args == [] and len(other_tags) == 0 and other_rest is not None:
            return other_rest.unify(self, sub)
        raise TypeError("Failed to unify %s and %s" % (self, tpack(other_rest, other_args, other_tags)))

class tvar:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'tvar(%r)' % self.name

    def __str__(self):
        return self.name

    def __eq__ (self, other):
        return self.name == other.name

    def rigidify(self):
        return tcon('%' + self.name, [])

    def freshen(self, prefix):
        return tvar(prefix + self.name)

    def has_var(self, name):
        return self.name == name

    def subst(self, sub):
        """
        >>> tvar('x').subst({'x': tvar('y')})
        tvar('y')
        >>> tvar('x').subst({'y': tvar('z')})
        tvar('x')
        """
        if self.name in sub:
            return sub[self.name]
        else:
            return self

    def unify(self, other, sub):
        """
        >>> tvar('x').unify(tvar('x'), {})
        tvar('x')
        >>> tvar('x').unify(tvar('y'), {})
        tvar('x')
        >>> tvar('x').unify(tcon('foo', []), {})
        tcon('foo')
        >>> tvar('x').unify(tcon('foo', []), {'x': tvar('y')})
        tcon('foo')
        >>> tvar('x').unify(tcon('foo', []), {'x': tcon('foo', [])})
        tcon('foo')
        >>> try: tvar('x').unify(tcon('foo', []), {'x': tcon('bar', [])})
        ... except TypeError as e: print(e)
        Failed to unify bar and foo.
        >>> try: tvar('x').unify(tcon('foo', [tvar('x')]), {})
        ... except TypeError as e: print(e)
        Failed to unify x and foo(x).
        """
        return other.unify_tvar(self.name, sub)

    def unify_tcon(self, other_name, other_args, sub):
        if self.name in sub:
            return sub[self.name].unify_tcon(other_name, other_args, sub)
        else:
            newself = tcon(other_name, [oarg.subst(sub) for oarg in other_args])
            if newself.has_var(self.name):
                raise TypeError("Failed to unify %s and %s." % (self.name, newself))
            var_sub = { self.name: newself }
            for v in sub:
                sub[v] = sub[v].subst(var_sub)
            sub[self.name] = newself
            return newself

    def unify_tvar(self, other_name, sub):
        if self.name == other_name:
            return self
        elif self.name in sub:
            return sub[self.name].unify_tvar(other_name, sub)
        elif other_name in sub:
            return sub[other_name].unify(self, sub)
        else:
            newself = tvar(other_name)
            var_sub = { self.name: newself }
            for v in sub:
                sub[v] = sub[v].subst(var_sub)
            sub[self.name] = newself
            return newself

    def unify_tpack(self, other_rest, other_args, other_tags, sub):
        if self.name in sub:
            return sub[self.name].unify_tpack(other_rest, other_args, other_tags, sub)
        elif other_args == [] and len(other_tags) == 0 and other_rest is not None:
            return self.unify(other_rest, sub)
        else:
            newself = tpack(other_rest, other_args, other_tags).subst(sub)
            if newself.has_var(self.name):
                raise TypeError("Failed to unify %s and %s." % (self.name, newself))
            var_sub = { self.name: newself }
            for v in sub:
                sub[v] = sub[v].subst(var_sub)
            sub[self.name] = newself
            return newself

class tpack:
    """
    >>> tpack()
    tpack()
    >>> tpack(None, [tvar('x')])
    tpack(None, [tvar('x')])
    >>> tpack(tvar('x'), [tvar('y')])
    tpack(tvar('x'), [tvar('y')])
    >>> tpack(tvar('x'), [tvar('y')]).unify(tpack(tvar('z'), [tcon('Int', []), tcon('Str', [])]), {})
    tpack(tvar('z'), [tcon('Int'), tcon('Str')])
    >>> tpack(tvar('x'), [tvar('y')]).unify(tpack(tvar('y'), [tcon('Int', []), tcon('Str', [])]), {})
    tpack(tcon('Str'), [tcon('Int'), tcon('Str')])
    >>> tpack(tvar('x'), [tvar('y')], {'+IO'})
    tpack(tvar('x'), [tvar('y')], {'+IO'})
    >>> tpack(tvar('x'), [tvar('y')], {'+IO'}).unify(tpack(tvar('x'), [tvar('y')], {'+Exit'}), {})
    tpack(tvar('x'), [tvar('y')], {'+Exit', '+IO'})
    """
    def __init__ (self, rest=None, args=(), tags=()):
        self.rest = rest
        self.args = list(args)
        self.tags = set(tags)
        while isinstance(self.rest, tpack):
            self.args = self.rest.args + self.args
            self.tags = self.rest.tags | self.tags
            self.rest = self.rest.rest

    def __repr__ (self):
        if self.rest is None and len(self.args) == 0 and len(self.tags) == 0:
            return 'tpack()'
        elif len(self.args) == 0 and len(self.tags) == 0:
            return 'tpack(%r)' % self.rest
        elif len(self.tags) == 0:
            return 'tpack(%r, %r)' % (self.rest, self.args)
        else:
            return 'tpack(%r, %r, {%s})' % (
                self.rest, self.args,
                ', '.join(sorted(repr(t) for t in self.tags)))

    def __str__ (self):
        argspart = ' '.join(str(a) for a in self.args + list(sorted(self.tags)))
        if self.rest is None:
            return '[ %s ]' % argspart
        else:
            return '[ *%s %s ]' % (self.rest, argspart)

    def __eq__ (self, other):
        return self.args == other.args and self.rest == other.rest and self.tags == other.tags

    def rigidify(self):
        if self.rest is None:
            return tpack(None, [a.rigidify() for a in self.args], self.tags)
        else:
            return tpack(self.rest.rigidify(), [a.rigidify() for a in self.args], self.tags)

    def freshen(self, prefix):
        if self.rest is None:
            return tpack(None, [a.freshen(prefix) for a in self.args], self.tags)
        else:
            return tpack(self.rest.freshen(prefix), [a.freshen(prefix) for a in self.args], self.tags)

    def has_var(self, name):
        if self.rest is None:
            return any(a.has_var(name) for a in self.args)
        else:
            return self.rest.has_var(name) or any(a.has_var(name) for a in self.args)

    def subst(self, sub):
        if self.rest is None:
            return tpack(None, [a.subst(sub) for a in self.args], self.tags)
        else:
            return tpack(self.rest.subst(sub), [a.subst(sub) for a in self.args], self.tags)

    def unify(self, other, sub):
        return other.unify_tpack(self.rest, self.args, self.tags, sub)

    def unify_tcon(self, other_name, other_args, sub):
        if len(self.args) == 0 and len(self.tags) == 0 and self.rest is not None:
            return self.rest.unify_tcon(other_name, other_args, sub)
        raise TypeError("Failed to unify %s and %s." % (self, tcon(other_name, other_args)))

    def unify_tvar(self, other_name, sub):
        return tvar(other_name).unify_tpack(self.rest, self.args, self.tags, sub)

    def unify_tpack(self, other_rest, other_args, other_tags, sub):
        if len(self.args) < len(other_args):
            return tpack(other_rest, other_args, other_tags
                    ).unify_tpack(self.rest, self.args, self.tags, sub)
        elif len(self.args) > len(other_args):
            if other_rest is None:
                raise TypeError("Failed to unify %s and %s" %
                    (self, tpack(other_rest, other_args, other_tags)))
            n = len(self.args) - len(other_args)
            (largs, rargs) = (self.args[:n], self.args[n:])
            newrest = other_rest.unify_tpack(self.rest, largs, self.tags, sub)
            newargs = []
            for (rarg, oarg) in zip(rargs, other_args):
                newargs.append(rarg.unify(oarg, sub))
            return tpack(newrest, newargs, newrest.tags).subst(sub)
        else:
            npack = tpack()
            npack.tags = (self.tags | other_tags) - (self.tags & other_tags)
            srest = self.rest if self.rest else npack
            orest = other_rest if other_rest else npack
            newrest = srest if srest is orest else srest.unify(orest, sub)
            newargs = []
            for (rarg, oarg) in zip(self.args, other_args):
                newargs.append(rarg.unify(oarg, sub))
            return tpack(newrest, newargs, self.tags | other_tags).subst(sub)

var_counter = 0
def fresh_var():
    global var_counter
    var_counter += 1
    return tvar('?' + str(var_counter))

##############################################################################
############################### ELABORATOR ###################################
##############################################################################

class module:
    def __init__(self):
        self.type_sigs = {}
        self.types = builtin_types.copy()
        self.prims = builtin_prims.copy()
        self.data_defs = {}
        self.word_sigs = builtin_word_sigs.copy()
        self.word_defs = builtin_word_defs.copy()
        self.assertions = []

    def has_type(self, name):
        return name in self.types

    def get_type(self, name, args):
        if name not in self.types:
            raise TypeError("Type %s not defined." % name)
        return self.types[name] (self, args)

    def has_prim(self, name):
        return name in self.prims

    def get_prim(self, name):
        if name not in self.prims:
            raise TypeError("Prim %s is not defined." % name)
        return self.prims[name]

    def get_word_sig (self, name):
        if name not in self.word_sigs:
            raise NameError("Word %s is not declared." % name)
        return self.word_sigs[name]

    def get_word_def (self, name):
        if name not in self.word_defs:
            raise NameError("Word %s is not defined." % name)
        return self.word_defs[name]

    def decl_word_sig (self, name, params, dom, cod):

        ps = []
        for (pname, pdom, pcod) in params:
            pdomte = type_elaborator(self)
            pcodte = type_elaborator(self)
            pdom.elab(pdomte)
            pcod.elab(pcodte)
            ps.append((pdomte.to_tpack(), pcodte.to_tpack()))

        domte = type_elaborator(self)
        codte = type_elaborator(self)
        dom.elab(domte)
        cod.elab(codte)

        sig = (ps, domte.to_tpack(), codte.to_tpack())

        if name in self.word_sigs:
            if self.word_sigs[name] != sig:
                raise TypeError("Word %s is already declared with different type." % name)
        else:
            self.word_sigs[name] = sig

    def decl_word_def (self, name, params, body):
        if name not in self.word_sigs:
            raise TypeError("Word %s is defined without type signature." % name)
        if name in self.word_defs:
            raise TypeError("Word %s is already defined." % name)

        (wargs, dom, cod) = self.word_sigs[name]

        if len(params) != len(wargs):
            raise TypeError(
                ("Definition of %s has the wrong number of arguments."
                +" Expected %d arguments but definition has %d.")
                % (name, len(wargs), len(params))
            )

        loc = {}
        for (pname, (i, (argdom, argcod))) in zip(params, enumerate(wargs)):
            adom = argdom.rigidify()
            acod = argcod.rigidify()
            loc[pname] = (i, adom, acod)

        elab = word_elaborator(self, dom.rigidify(), loc)
        func = body.elab(elab)
        cod2 = elab.dom
        cod.rigidify().unify(cod2, {})
        extra_tags = cod2.tags - cod.tags
        if len(extra_tags):
            raise TypeError("Word %s missing %s tag%s in output type." %
                (name, ' '.join(sorted(extra_tags)),
                ('s' if len(extra_tags) != 1 else '')))
        self.word_defs[name] = func

    def decl_assertion (self, lineno, lhs, rhs):
        orig = tpack(fresh_var())

        elab1 = word_elaborator(self, orig)
        elab2 = word_elaborator(self, orig)
        elab1.sub = elab2.sub

        lhsf = lhs.elab(elab1)
        rhsf = rhs.elab(elab2)

        elab1.dom.unify(elab2.dom, elab1.sub)
        orig = orig.subst(elab1.sub)
        self.assertions.append((lineno, orig, lhsf, rhsf))

    def decl_type_sig (self, lineno, name, params):
        if name in self.type_sigs:
            if len(params) != len(self.type_sigs[name]):
                raise TypeError(
                    "Line %d: Type %s declared previously, but with different number of params."
                    % (lineno, name)
                )
            return

        elif name in self.types:
            raise TypeError(
                "Line %d: Type %s defined twice."
                % (lineno, name)
            )

        def ft(mod, args):
            if len(args) != len(params):
                raise TypeError("Type %s expects %d args, but got %d args."
                    % (name, len(params), len(args)))

            ta = []
            for arg in args:
                te = type_elaborator(mod)
                arg.elab(te)
                tp = te.to_tpack()
                if tp.rest is not None or len(tp.args) != 1:
                    raise TypeError("Type %s received bad argument." % name)
                ta.append(tp.args[0])

            return tcon(name, ta)

        self.types[name] = ft
        self.type_sigs[name] = params

    def decl_data_def (self, lineno, name, params, wordsigs):
        if name in self.type_sigs:
            tsparams = self.type_sigs[name]
            if len(tsparams) != len(params):
                raise TypeError(
                    "Line %d: Type %s takes %d params in type sig but %d params in type def."
                    % (lineno, name, len(tsparams), len(params))
                )

        if name in self.data_defs or (name in self.types and name not in self.type_sigs):
            raise TypeError(
                "Line %d: Type %s defined twice."
                % (lineno, name)
            )

        if name[0] == '+':
            raise SyntaxError("Line %d: Can't declare tag type %s as data."
                % (lineno, name))

        for wordsig in wordsigs:
            if len(wordsig.cod.atoms) != 1:
                raise SyntaxError(
                    "Line %d: Constructor must return a single value."
                    % lineno
                )
            if wordsig.cod.atoms[0].name.code != name:
                raise SyntaxError(
                    "Line %d: Constructor must return value of constructed type."
                    % lineno
                )
            if len(wordsig.cod.atoms[0].args) != len(params):
                raise SyntaxError(
                    "Line %d: Mismatched number of parameters in constructor output type. Expected %d but got %d."
                    % (lineno, len(params), len(wordsig.cod.atoms[0].args))
                )

            argnames = set()
            for arg in wordsig.cod.atoms[0].args:
                if len(arg.atoms) != 1:
                    raise SyntaxError(
                        "Line %d: Constructor %s output type not valid."
                        % (lineno, wordsig.name.code)
                    )
                arg = arg.atoms[0]
                if not isinstance(arg, word):
                    raise SyntaxError(
                        "Line %d: Constructor %s output type not valid."
                        % (lineno, wordsig.name.code)
                    )
                if not (arg.name not in argnames
                    and arg.name.code[0].islower()
                    and len(arg.args) == 0):
                    raise SyntaxError(
                        "Line %d: Constructor %s requires GADTs, not supported."
                        % (lineno, wordsig.name.code)
                    )
                argnames.add(arg.name)

            if (len(wordsig.dom.atoms) > 0
                and isinstance(wordsig.dom.atoms[0], word)
                and wordsig.dom.atoms[0].name.code[0] == '*'):
                raise SyntaxError(
                    "Line %d: Constructor must take a fixed number of inputs."
                    % lineno
                )

            for wsatom in wordsig.dom.atoms:
                if isinstance(wsatom, word) and wsatom.name.code[0] == '+':
                    raise SyntaxError(
                        "Line %d: Can't take tags in constructor."
                        % wsatom.name.lineno
                    )

        def ft(mod, args):
            if len(args) != len(params):
                raise TypeError("Type %s expects %d args, but got %d args."
                    % (name, len(params), len(args)))

            ta = []
            for arg in args:
                te = type_elaborator(mod)
                arg.elab(te)
                tp = te.to_tpack()
                if tp.rest is not None or len(tp.args) != 1 or len(tp.tags) != 0:
                    raise TypeError("Type %s received bad argument." % name)
                ta.append(tp.args[0])

            return tcon(name, ta)

        self.types[name] = ft

        def addworddef(wordsig):
            wname = wordsig.name.code
            wlen = len(wordsig.dom.atoms)
            def wfn(e, *args):
                ws = []
                for i in range(wlen):
                    ws.append(e.pop())
                e.push((wname, tuple(args), tuple(ws[::-1])))
            if wname in self.word_defs:
                raise TypeError("Constructor %s already defined." % wname)
            self.word_defs[wname] = wfn

        self.data_defs[name] = {}
        for wordsig in wordsigs:
            wordsig.decl(self)
            addworddef(wordsig)
            self.data_defs[name][wordsig.name.code] = self.word_sigs[wordsig.name.code]

    def decl_expr (self, expr):
        raise SyntaxError("Bare expression not supported.")

    def check_assertions(self):
        for a in self.assertions:
            self.check_assertion(a)

    def check_assertion(self, assn):
        (lineno, dom, f0, f1) = assn
        tries = 1 + 10 * len(dom.args)
        for try_number in range(tries):
            e0 = env()
            e1 = env()
            vs = []
            for d in dom.args:
                v = self.arbitrary(d, try_number)
                vs.append(v)
                e0.push(v)
                e1.push(v)
            f0(e0)
            f1(e1)
            e0.run()
            e1.run()
            if e0.stack != e1.stack:
                raise ValueError(
                    ("Assertion failed at line %d:"
                    +" input = [%s], LHS = [%s], RHS = [%s].")
                    % ( lineno
                      , ' '.join(map(repr, vs))
                      , ' '.join(map(repr, e0.stack))
                      , ' '.join(map(repr, e1.stack)) ))

    def arbitrary(self, t, n=10):
        if isinstance(t, tvar):
            return random.randint(-n, n)
        elif isinstance(t, tcon):
            gname = t.name+'.generate'
            if gname in self.word_sigs:
                (ps, dom, cod) = self.word_sigs[gname]
                if len(ps):
                    raise TypeError(
                        "%s cannot take args."
                        % gname
                    )
                if dom.rest is not None:
                    raise TypeError(
                        "%s must take fixed number of args."
                        % gname
                    )

                if not (cod.rest is None and len(cod.args) == 1):
                    raise TypeError(
                        "%s must return a single value."
                        % gname
                    )

                if not (isinstance(cod.args[0], tcon) and cod.args[0].name == t.name):
                    raise TypeError(
                        "%s must return value of type %s."
                        % (gname, t.name)
                    )
                fun = self.get_word_def(gname)
                sub = {}
                t.unify(cod.args[0].freshen(fresh_var().name), sub)
                e = env()
                for d in dom.args:
                    e.push(self.arbitrary(d.subst(sub), n))
                e.copush(fun)
                e.run()
                return e.pop()
            elif t.name == 'List' and len(t.args) == 1:
                xt = t.args[0]
                xs = []
                for i in range(random.randint(0, n//2)):
                    xs.append(self.arbitrary(xt, n//2))
                return xs
            elif t.name == 'Int' and t.args == []:
                return random.randint(-n, n)
            elif t.name == 'Bool' and t.args == []:
                return random.choice([True, False])
            elif t.name == 'Str' and t.args == []:
                l = random.randint(0, n)
                return ''.join(chr(random.randint(1,128)) for i in range(l))
        elif isinstance(t, tpack):
            vs = []
            for t2 in t.args:
                vs.append(self.arbitrary(t2, n))
            return tuple(vs)

        raise TypeError("Don't know how to generate value of type %s." % t)

class type_elaborator:
    def __init__(self, mod):
        self.mod = mod
        self.rest = None
        self.args = []
        self.tags = set()

    def to_tpack(self):
        return tpack(self.rest, self.args, self.tags)

    def elab_push_int(self, value):
        raise TypeError("Expected a type but got an int.")

    def elab_push_str(self, value):
        raise TypeError("Expected a type but got a string.")

    def elab_word(self, name, args):
        if  (len(name) >= 2 and '*' == name[0] and 'a' <= name[1] <= 'z'
                and not self.mod.has_type(name)):
            if len(args):
                raise TypeError("Stack type var with args not supported.")
            elif self.args:
                raise TypeError("Can't have a stack type var after a type.")
            elif self.rest:
                raise TypeError("Can't have a stack type var after another stack type var.")
            else:
                self.rest = tvar(name[1:])

        elif (len(name) >= 2 and '+' == name[0] and 'A' <= name[1] <= 'Z'):
            if len(args):
                raise TypeError("Tag with args not supported.")
            if not self.mod.has_type(name):
                raise TypeError("Unknown tag type %s." % name)
            self.tags.add(name)

        elif 'a' <= name[0] <= 'z' and not self.mod.has_type(name):
            if len(args):
                raise TypeError("Type var with args not currently supported.")
            else:
                self.args.append(tvar(name))
        else:
            self.args.append(self.mod.get_type(name, args))

    def elab_expr(self, atoms):
        for atom in atoms:
            atom.elab(self)

class word_elaborator:
    def __init__(self, mod, dom, loc=None):
        self.mod = mod
        self.dom = dom
        self.loc = loc or {}
        self.sub = {}

    def elab_push_int(self, value):
        self.dom = tpack(self.dom, [tcon('Int', [])])
        return lambda p, *args: p.push(value)

    def elab_push_str(self, value):
        self.dom = tpack(self.dom, [tcon('Str', [])])
        return lambda p, *args: p.push(value)

    def elab_word(self, name, args):
        if name in self.loc:
            (nargi, dom, cod) = self.loc[name]

            wargs = []
            def mkfn(fs):
                def fn(p, *nargs):
                    p.copush(nargs[nargi])
                return fn

        elif self.mod.has_prim(name):
            return self.mod.get_prim(name) (self, args)

        else:
            (wargs, dom, cod) = self.mod.get_word_sig(name)

            def mkfn(fs):
                def fn(p, *nargs):
                    def pushedfn(e):
                        d = self.mod.get_word_def(name)
                        dargs = []
                        for f in fs:
                            def g(f): return lambda e2: f(e2, *nargs)
                            dargs.append(g(f))
                        d(e, *dargs)
                    p.copush(pushedfn)
                return fn

        if len(args) != len(wargs):
            raise SyntaxError("%s: expected %d args but got %d args"
                % (name, len(wargs), len(args)))

        if dom.rest is None and cod.rest is None:
            ovar = fresh_var()
            dom = tpack(ovar, dom.args, dom.tags)
            cod = tpack(ovar, cod.args, cod.tags)

        prefix = fresh_var().name + '.'
        self.dom.unify(dom.freshen(prefix), self.sub)

        fs = []
        for (arg, (wargdom, wargcod)) in zip(args, wargs):
            adom = wargdom.freshen(prefix).subst(self.sub)
            acod = wargcod.freshen(prefix).subst(self.sub)
            elab2 = word_elaborator(self.mod, adom, self.loc)
            elab2.sub = self.sub
            fs.append(arg.elab(elab2))
            elab2.dom.unify(acod, self.sub)

        self.dom = cod.freshen(prefix).subst(self.sub)

        # then prune the sub
        vs = list(self.sub.keys())
        for v in vs:
            if v[:len(prefix)] == prefix:
                del self.sub[v]

        return mkfn(fs)

    def elab_expr(self, atoms):
        fns = []
        for atom in atoms:
            fns.append(atom.elab(self))

        def f(p, *args):
            def g(fn):
                return (lambda e: fn(e, *args))
            for fn in fns[::-1]:
                p.copush(g(fn))
        return f

class env:
    def __init__(self):
        self.stack  = []
        self.rstack = []
        self.data   = {}

    def push(self, v):
        self.stack.append(v)

    def pop(self):
        return self.stack.pop()

    def copush(self, r):
        self.rstack.append(r)

    def copop(self):
        return self.rstack.pop()

    def drop(self):
        self.pop()

    def dup(self):
        self.push(self.stack[-1])

    def swap(self):
        b = self.pop()
        a = self.pop()
        self.push(b)
        self.push(a)

    def dip(self, f):
        x = self.pop()
        self.copush(lambda env: env.push(x))
        self.copush(f)

    def w_if(self, ft, ff):
        x = self.pop()
        if x:
            self.copush(ft)
        else:
            self.copush(ff)

    def step(self):
        if self.rstack:
            w = self.copop()
            if callable(w):
                w(self)
            else:
                raise TypeError("Unknown item on return stack: %r" % w)
            return True
        return False

    def run(self, timeout=1000000):
        r = 0
        while self.step():
            r += 1
            if r > timeout:
                raise ValueError("Ran out of time (infinite loop?).")

    def show_stack(self):
        print(' '.join(repr(s) for s in self.stack))

    def get_data(self):
        x = self.pop()
        self.push(self.data[x])

    def set_data(self):
        x = self.pop()
        y = self.pop()
        self.data[x] = y

    def save_data(self, f):
        x = self.pop()
        y = self.data[x] if x in self.data else None
        def g(e):
            if y is None:
                del e.data[x]
            else:
                e.data[x] = y
        self.copush(g)
        self.copush(f)

##############################################################################
################################ BUILTINS ####################################
##############################################################################

def type0 (name, t):
    def f(mod, args):
        if len(args) > 0:
            raise TypeError("%s takes no arguments." % name)
        return t
    return f

def type1 (name, t):
    def f(mod, args):
        if len(args) != 1:
            raise TypeError("%s takes one argument." % name)
        e = type_elaborator(mod)
        args[0].elab(e)
        p = e.to_tpack()
        if not (p.rest is None and len(p.args) == 1):
            raise TypeError("%s expects a value type argument, not a stack type." % name)
        return t(p.args[0])
    return f

def mktpack (mod, args):
    if len(args) != 1:
        raise TypeError("Pack takes one argument.")
    e = type_elaborator(mod)
    args[0].elab(e)
    return e.to_tpack()

def match (elab, args):
    match = []
    if len(elab.dom.args) < 1:
        raise TypeError("Not a valid domain for match: [%s]" % elab.dom)
    domt = elab.dom.args[-1]
    if not (isinstance(domt, tcon) and domt.name in elab.mod.data_defs):
        raise TypeError("Not a valid domain for match: [%s]" % elab.dom)
    dname = domt.name
    dargs = domt.args
    dcons = elab.mod.data_defs[dname]
    rules = {}
    else_rule = None
    outtp = tpack(fresh_var())

    for arg in args:
        line = list (arg.split_on('->'))
        if len(line) != 2:
            raise SyntaxError("Expected -> in argument to match.")
        lhs, rhs = tuple(line)
        if not (len(lhs.atoms) == 1 and isinstance(lhs.atoms[0], word)):
            raise SyntaxError(
                "Expected a single constructor on LHS of -> in match, but got %s"
                % lhs
            )
        cname = lhs.atoms[0].name.code
        cargs = lhs.atoms[0].args
        if cname != '_' and cname not in dcons:
            raise TypeError(
                "Error: %s is not a constructor for type %s."
                % (cname, dname)
            )

        if else_rule is not None:
            raise SyntaxError("Case %s is made redundant by default case in match." % cname)

        if cname == '_':
            celab = word_elaborator(elab.mod, elab.dom, elab.loc)
            celab.sub = elab.sub
            else_rule = rhs.elab(celab)
            outtp = outtp.unify(celab.dom, elab.sub)
            continue

        (cparams, cdomt, ccodt) = dcons[cname]

        if cname in rules:
            raise SyntaxError("Constructor %s appears twice in match." % cname)

        if len(cargs) != len(cparams):
            raise TypeError(
                "Expected %d params for %s constructor in match but got %d params."
                % (len(cparams), cname, len(cargs)))

        cargvars = []
        for carg in cargs:
            if len(carg.atoms) != 1:
                raise SyntaxError(
                    "Expected single atom in %s constructor param in match."
                    % cname
                )
            cargatom = carg.atoms[0]
            if not (isinstance(cargatom, word) and len(cargatom.args) == 0):
                raise SyntaxError(
                    "Expected single variable in %s constructor param in match."
                    % cname
                )
            cargvar = cargatom.name.code
            if cargvar in elab.loc:
                raise TypeError(
                    "%s constructor param shadows local variable %s."
                    % (cname, cargvar)
                )
            if cargvar in cargvars:
                raise SyntaxError(
                    "%s constructor param %s appears twice."
                    % (cname, cargvar)
                )
            cargvars.append(cargvar)

        prefix = fresh_var()
        pvar   = fresh_var()
        cdomt = tpack(pvar, cdomt.args).freshen(prefix.name)
        ccodt = tpack(pvar, ccodt.args).freshen(prefix.name)
        ccodt = ccodt.unify(elab.dom, elab.sub)
        cdomt = cdomt.subst(elab.sub)
        cloc = elab.loc.copy()
        for (cvar, (i, (cpdom, cpcod))) in zip(cargvars, enumerate(cparams)):
            cloc[cvar] = (len(elab.loc) + i,
                cpdom.freshen(prefix.name).subst(elab.sub),
                cpcod.freshen(prefix.name).subst(elab.sub))
        celab = word_elaborator(elab.mod, cdomt, cloc)
        celab.sub = elab.sub
        rules[cname] = rhs.elab(celab)
        outtp = outtp.unify(celab.dom, elab.sub)

    missing_rules = set(dcons)
    for cname in rules:
        missing_rules.remove(cname)
    if else_rule is None:
        if missing_rules:
            raise TypeError( 'Missing rule for constructor %s in match'
                % ', '.join(sorted(missing_rules)) )
    else:
        if not missing_rules:
            raise TypeError( 'Redundant default case in match.' )

    elab.dom = outtp
    def outfn(e, *args):
        v = e.pop()
        if not isinstance(v, tuple) or v[0] not in dcons:
            print('BOOTSTRAP BUG: RUNTIME TYPE ERROR: Expected a', dname, 'but got', repr(v), file=sys.stderr)
            print(e.stack, file=sys.stderr)
            sys.exit(1)

        if v[0] in rules:
            f = rules[v[0]]
            fargs = list(args) + list(v[1])
            for x in v[2]:
                e.push(x)
            f(e, *fargs)
        elif else_rule is not None:
            e.push(v)
            else_rule(e, *args)
    return outfn

#
# lam( x_1 ... x_n -> args )
#

def lam (elab, args):
    if len(args) != 1:
        raise SyntaxError("Expected a single argument to lambda.")
    line = list(args[0].split_on('->'))
    if len(line) < 2:
        raise SyntaxError("Expected -> in argument to lambda.")
    if len(line) > 2:
        raise SyntaxError("Expected only one -> in argument to lambda.")
    lhs, rhs = line

    names = []

    for atom in lhs.atoms:
        if not (isinstance(atom, word) and len(atom.args) == 0):
            raise SyntaxError("Expected lambda param to be simple variable name.")
        name = atom.name.code
        if name in elab.loc:
            raise TypeError(
                "Param in lambda shadows local variable %s."
                % name
            )
        if name in names:
            raise SyntaxError(
                "Param in lambda appears twice: %s"
                % name
            )
        names.append(name)

    trest = fresh_var()
    targs = [fresh_var() for name in names]
    elab.dom = elab.dom.unify(tpack(trest, targs), elab.sub)

    floc = elab.loc.copy()
    for (name, (i, targ)) in zip(names, enumerate(targs)):
        floc[name] = (i + len(elab.loc), tpack(None), tpack(None, [targ.subst(elab.sub)]))
    felab = word_elaborator(elab.mod, trest.subst(elab.sub), floc)
    felab.sub = elab.sub
    f = rhs.elab(felab)
    elab.dom = felab.dom.subst(felab.sub)

    def mkearg(x):
        def h(e):
            e.push(x)
        return h

    def g (e, *args):
        eargs = []
        for name in names:
            eargs.append(mkearg(e.pop()))
        args = list(args) + eargs[::-1]
        f (e, *args)

    return g

#
# cond( p_1 -> t_1, p_2 -> t_2, ... , p_n -> t_n, e ) : *a -- *b
#   where
#     p_i : *a -- *a Bool  with  p_i drop == id
#     t_i : *a -- *b
#     e   : *a -- *b
#
# Run p_1, p_2, ... p_n in succession until the first one returns
# true. If p_i returns true, then perform t_i. Otherwise, if all
# of p_1, p_2, ..., p_n return false, perfarm e.
#
# This is equivalent to the chained if expression:
#
#   p_1 if( t_1, p_2 if( t_2, ... p_n if( t_n, e ) ... ) )
#
def cond (elab, args):
    if len(args) < 1:
        raise SyntaxError("Expected at least one argument to cond.")
    rules = []
    odom = elab.dom
    lcod = tpack(odom, [tbool])
    rcod = tpack(fresh_var())
    otags = set()

    for arg in args[:-1]:
        line = list(arg.split_on('->'))
        if len(line) != 2:
            raise SyntaxError("Expected -> in argument to cond.")
        lhs, rhs = tuple(line)

        elab.dom = odom
        lhsfn = lhs.elab(elab)
        lcod = lcod.unify(elab.dom, elab.sub)

        elab.dom = odom
        rhsfn = rhs.elab(elab)
        rcod = rcod.unify(elab.dom, elab.sub)
        otags = otags | rcod.tags
        rules.append((lhsfn, rhsfn))

    elab.dom = tpack(odom.rest, odom.args, odom.tags | otags)
    elsefn = args[-1].elab(elab)
    rcod = rcod.unify(elab.dom, elab.sub)

    def outfn(e, *vs):

        def mkpfn(i):
            def f(e):
                if i < len(rules):
                    e.copush(mktfn(i))
                    rules[i][0](e, *vs)
                else:
                    elsefn(e, *vs)
            return f

        def mktfn(i):
            def f(e):
                b = e.pop()
                if b:
                    rules[i][1](e, *vs)
                else:
                    e.copush(mkpfn(i+1))
            return f

        e.copush(mkpfn(0))

    return outfn

tint  = tcon('Int')
tstr  = tcon('Str')
tbool = tcon('Bool')
tlist = lambda t: tcon('List', [t])

builtin_types = {
    'Int':  type0('Int', tint),
    'Str':  type0('Str', tstr),
    'Bool': type0('Bool', tbool),
    'List': type1('List', tlist),
    'Pack': mktpack,
}

builtin_prims = {
    'match': match,
    'cond': cond,
    'lambda': lam,
}

def word1 (f):
    def w (e):
        a = e.pop()
        e.push(f(a))
    return w

def word12 (f):
    def w (e):
        a = e.pop()
        x, y = f(a)
        e.push(x)
        e.push(y)
    return w

def word2 (f):
    def w(e):
        b = e.pop()
        a = e.pop()
        e.push(f(a,b))
    return w

def word22 (f):
    def w(e):
        b = e.pop()
        a = e.pop()
        x,y = f(a,b)
        e.push(x)
        e.push(y)
    return w

def intuple (e, f):
    newstack = list(e.pop())
    oldstack = e.stack
    e.stack = newstack
    def restore_stack(e2):
        oldstack.append(tuple(e2.stack))
        e2.stack = oldstack
    e.copush(restore_stack)
    e.copush(f)

def listmap (e, f):
    xs = e.pop()
    def g(x): return lambda e: e.push(x)
    def h(e):
        ys = e.stack[-len(xs):]
        del e.stack[-len(xs):]
        e.push(ys)
    e.copush(h)
    for x in reversed(xs):
        e.copush(f)
        e.copush(g(x))

def listfor (e, f):
    xs = e.pop()
    def g(x): return lambda e: e.push(x)
    for x in reversed(xs):
        e.copush(f)
        e.copush(g(x))

def unsafe_panic (e):
    m = e.pop()
    print(m, file=sys.stderr)
    print('\nstack: ' + ' '.join(map(repr, e.stack)), file=sys.stderr)
    sys.exit(1)

def unsafe_trace (e):
    m = e.pop()
    print(m, file=sys.stderr)

def unsafe_print (e):
    m = e.pop()
    print(m)

def unsafe_listdir (e):
    m = e.pop()
    e.push(os.listdir(m))

def unsafe_walk (e):
    m = e.pop()
    e.push(list(os.walk(m)))

def unsafe_isdir(e):
    m = e.pop()
    e.push(os.path.isdir(m))

def unsafe_isfile(e):
    m = e.pop()
    e.push(os.path.isfile(m))

def unsafe_read(e):
    m = e.pop()
    with open(m) as fp:
        v = fp.read()
    e.push(v)

def unsafe_write(e):
    m = e.pop()
    v = e.pop()
    with open(m, 'w') as fp:
        fp.write(v)

def unsafe_append(e):
    m = e.pop()
    v = e.pop()
    with open(m, 'a') as fp:
        fp.write(v)

recache = {}
def rematch(a,b):
    if b not in recache:
        recache[b] = re.compile(b, re.MULTILINE)
    r = recache[b]
    x = r.match(a)
    if x is None:
        return (0, False)
    return (len(x.group(0)), True)

builtin_word_sigs = {
    # basic
    '_prim_dup':  ([], tpack(None, [tvar('b')]), tpack(None, [tvar('b'), tvar('b')])),
    '_prim_drop': ([], tpack(None, [tvar('b')]), tpack(None)),
    '_prim_swap': ([], tpack(None, [tvar('b'), tvar('c')]), tpack(None, [tvar('c'), tvar('b')])),
    '_prim_dip':  ([(tpack(tvar('a')), tpack(tvar('b')))],
            tpack(tvar('a'), [tvar('c')]),
            tpack(tvar('b'), [tvar('c')])),

    # bool
    '_prim_bool_true':  ([], tpack(), tpack(None, [tbool])),
    '_prim_bool_false': ([], tpack(), tpack(None, [tbool])),
    '_prim_bool_if': ([(tpack(tvar('a')), tpack(tvar('b')))
           ,(tpack(tvar('a')), tpack(tvar('b')))],
           tpack(tvar('a'), [tbool]), tpack(tvar('b'))),

    # int
    '_prim_int_add': ([], tpack(None, [tint, tint]), tpack(None, [tint])),
    '_prim_int_sub': ([], tpack(None, [tint, tint]), tpack(None, [tint])),
    '_prim_int_mul': ([], tpack(None, [tint, tint]), tpack(None, [tint])),
    '_prim_int_div': ([], tpack(None, [tint, tint]), tpack(None, [tint])),
    '_prim_int_mod': ([], tpack(None, [tint, tint]), tpack(None, [tint])),
    '_prim_int_lt': ([], tpack(None, [tint, tint]), tpack(None, [tbool])),
    '_prim_int_eq': ([], tpack(None, [tint, tint]), tpack(None, [tbool])),

    # str
    '_prim_str_cat': ([], tpack(None, [tstr, tstr]), tpack(None, [tstr])),
    '_prim_str_break': ([], tpack(None, [tstr, tint]), tpack(None, [tstr, tstr])),
    '_prim_str_len': ([], tpack(None, [tstr]), tpack(None, [tint])),
    '_prim_str_to_codepoint': ([], tpack(None, [tstr]), tpack(None, [tint])),
    '_prim_str_from_codepoint': ([], tpack(None, [tint]), tpack(None, [tstr])),
    '_prim_str_elem': ([], tpack(None, [tstr, tstr]), tpack(None, [tbool])),
    '_prim_str_rematch': ([], tpack(None, [tstr, tstr]), tpack(None, [tint, tbool])),

    # tuple
    '_prim_tuple_intuple': ([(tpack(tvar('a')), tpack(tvar('b')))],
                            tpack(None, [tpack(tvar('a'))]),
                            tpack(None, [tpack(tvar('b'))])),
    '_prim_tuple_pack2'  : ([], tpack(None, [tvar('a'), tvar('b')]),
                            tpack(None, [tpack(None, [tvar('a'), tvar('b')])])),
    '_prim_tuple_unpack2': ([], tpack(None, [tpack(None, [tvar('a'), tvar('b')])]),
                            tpack(None, [tvar('a'), tvar('b')])),

    # list
    '_prim_list_nil': ([], tpack(None), tpack(None, [tlist(tvar('a'))])),
    '_prim_list_cons': ([], tpack(None, [tlist(tvar('a')), tvar('a')]), tpack(None, [tlist(tvar('a'))])),
    '_prim_list_cat': ([], tpack(None, [tlist(tvar('a')), tlist(tvar('a'))]), tpack(None, [tlist(tvar('a'))])),
    '_prim_list_len': ([], tpack(None, [tlist(tvar('a'))]), tpack(None, [tint])),
    '_prim_list_at': ([], tpack(None, [tlist(tvar('a')), tint]), tpack(None, [tvar('a')])),
    '_prim_list_break': ([], tpack(None, [tlist(tvar('a')), tint]),
        tpack(None, [tlist(tvar('a')), tlist(tvar('a'))])),
    '_prim_list_map': ([(tpack(None, [tvar('a')]), tpack(None, [tvar('b')]))],
        tpack(None, [tlist(tvar('a'))]), tpack(None, [tlist(tvar('b'))])),
    '_prim_list_for': ([(tpack(tvar('a'), [tvar('b')]), tpack(tvar('a')))],
        tpack(tvar('a'), [tlist(tvar('b'))]), tpack(tvar('a'))),

    # unsafe
    '_prim_unsafe_panic':   ([], tpack(tvar('a'), [tstr]), tpack(tvar('b'))),
    '_prim_unsafe_trace':   ([], tpack(None, [tstr]), tpack(None)),
    '_prim_unsafe_print':   ([], tpack(None, [tstr]), tpack(None)),
    '_prim_unsafe_listdir': ([], tpack(None, [tstr]), tpack(None, [tlist(tstr)])),
    '_prim_unsafe_walk':    ([], tpack(None, [tstr]),
        tpack(None, [tlist(tpack(None, [tstr, tlist(tstr), tlist(tstr)]))])),
    '_prim_unsafe_isdir':   ([], tpack(None, [tstr]), tpack(None, [tbool])),
    '_prim_unsafe_isfile':  ([], tpack(None, [tstr]), tpack(None, [tbool])),
    '_prim_unsafe_read':    ([], tpack(None, [tstr]), tpack(None, [tstr])),
    '_prim_unsafe_write':   ([], tpack(None, [tstr, tstr]), tpack(None)),
    '_prim_unsafe_append':  ([], tpack(None, [tstr, tstr]), tpack(None)),
    '_prim_unsafe_coerce':  ([], tpack(tvar('a')), tpack(tvar('b'))),
    '_prim_unsafe_hash':    ([], tpack(None, [tvar('a')]), tpack(None, [tint])),
    '_prim_unsafe_env_get': ([], tpack(None, [tstr]), tpack(None, [tvar('a')])),
    '_prim_unsafe_env_set': ([], tpack(None, [tvar('a'), tstr]), tpack(None, [])),
    '_prim_unsafe_env_save': ([(tpack(tvar('a')), tpack(tvar('b')))],
        tpack(tvar('a'), [tstr]), tpack(tvar('b'))),
    '_prim_unsafe_exit':    ([], tpack(tvar('a'), [tint]), tpack(tvar('b'))),
}

builtin_word_defs = {
    # basic
    '_prim_dup':        env.dup,
    '_prim_drop':       env.drop,
    '_prim_swap':       env.swap,
    '_prim_dip':        env.dip,

    # tuple
    '_prim_tuple_intuple': intuple,
    '_prim_tuple_pack2':   word2(lambda a,b: (a,b)),
    '_prim_tuple_unpack2': word12(lambda p: p),

    # bool
    '_prim_bool_true':  (lambda env: env.push(True)),
    '_prim_bool_false': (lambda env: env.push(False)),
    '_prim_bool_if':    env.w_if,

    # int
    '_prim_int_add':    word2(lambda a,b: a + b),
    '_prim_int_sub':    word2(lambda a,b: a - b),
    '_prim_int_mul':    word2(lambda a,b: a * b),
    '_prim_int_mod':    word2(lambda a,b: a % b),
    '_prim_int_div':    word2(lambda a,b: a // b),
    '_prim_int_lt':     word2(lambda a,b: a < b),
    '_prim_int_eq':     word2(lambda a,b: a == b),

    # str
    '_prim_str_cat':    word2(lambda a,b: a + b),
    '_prim_str_break':  word22(lambda a,b: (a[:b], a[b:]) if b >= 0 else ('', a)),
    '_prim_str_len':    word1(len),
    '_prim_str_from_codepoint':    word1(chr),
    '_prim_str_to_codepoint':    word1(ord),
    '_prim_str_elem':    word2(lambda a,b: a in b),
    '_prim_str_rematch': word22(rematch),

    # list
    '_prim_list_nil'  : (lambda env: env.push([])),
    '_prim_list_cons' : word2(lambda a,b: a+[b]),
    '_prim_list_cat'  : word2(lambda a,b: a+b),
    '_prim_list_len'  : word1(len),
    '_prim_list_at'   : word2(lambda a,b: a[b]),
    '_prim_list_break': word22(lambda a,b: (a[:b], a[b:]) if b >= 0 else ([], a)),
    '_prim_list_map'  : listmap,
    '_prim_list_for'  : listfor,

    # unsafe
    '_prim_unsafe_panic':    unsafe_panic,
    '_prim_unsafe_trace':    unsafe_trace,
    '_prim_unsafe_print':    unsafe_print,
    '_prim_unsafe_listdir':  unsafe_listdir,
    '_prim_unsafe_walk':     unsafe_walk,
    '_prim_unsafe_isdir':    unsafe_isdir,
    '_prim_unsafe_isfile':   unsafe_isfile,
    '_prim_unsafe_read':     unsafe_read,
    '_prim_unsafe_write':    unsafe_write,
    '_prim_unsafe_append':   unsafe_append,
    '_prim_unsafe_coerce':   word1(lambda a: a),
    '_prim_unsafe_hash':     word1(hash),
    '_prim_unsafe_env_get':  env.get_data,
    '_prim_unsafe_env_set':  env.set_data,
    '_prim_unsafe_env_save': env.save_data,
    '_prim_unsafe_exit':     word1(sys.exit),
}

##############################################################

if __name__ == '__main__':
    if list(sorted(builtin_word_sigs)) != list(sorted(builtin_word_defs)):
        raise ValueError("Builtins are mismatched.")

    main()


