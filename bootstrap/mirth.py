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

import random

def main():
    import sys

    try:
        i = sys.argv.index('--no-prelude')
        del sys.argv[i]
        with_prelude = False
    except ValueError:
        with_prelude = True

    if len(sys.argv) == 1:
        repl(with_prelude=with_prelude)
    elif sys.argv[1:] == ['--doctest']:
        import doctest
        doctest.testmod()
    else:
        interpret(sys.argv[1], sys.argv[2:], with_prelude=with_prelude)

def interpret(path, args, with_prelude=True):
    with open(path) as fp:
        decls = parse(fp)

    m = module()
    for d in decls:
        d.decl(m)

    m.check_assertions()

def repl(with_prelude=True):
    # REPL banner
    print()
    print('Mirth Bootstrap Interpreter v0.0.0: CAVEAT USOR.')
    print()
    print('Thank you to our patrons on https://patreon.com/mirth_lang')
    print('And a special thanks to Benjohn.')
    print()

    m = module()
    e = env(m)
    l = word_elaborator(m, tpack())

    try:
        keep_going = True
        while keep_going:
            code = input(">>> ")
            if code.strip() == 'bye': break
            try:
                decls = parse(code)
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
################################# LEXING #####################################
##############################################################################

def tokenize (code):
    r'''tokenize (file or string) -> token_generator

    Lexify a mirth codebase.

    >>> list(tokenize('foo bar'))
    [token('foo', 1), token('bar', 1), token('\n', 1)]

    '''

    if isinstance(code, str):
        lines = code.split('\n')
    else:
        lines = list(code)

    for i, line in enumerate(lines):
        toks = (line.replace('(', ' ( ')
                    .replace(')', ' ) ')
                    .replace(',', ' , ')
                    .split())
        emitted = False
        for tok in toks:
            if tok == '#': # comment
                break
            elif tok:
                emitted = True
                yield token(code=tok, lineno=i+1)
        if emitted:
            yield token(code='\n', lineno=i+1)

# tokens with special meaning
reserved = {'\n', '(', ')', ',', ':', '--', '=', ':=', '==', 'type', 'data', 'end'}

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
    def is_type    (self): return self.type == 'type'
    def is_data    (self): return self.code == 'data'
    def is_end     (self): return self.code == 'end'

    def is_int(self):
        try:
            int(self.code)
        except:
            return False
        return True

    def is_name(self):
        return not (self.code in reserved
                 or self.is_int())

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
            star(fmapseq((lambda a,b: a), sep, p))
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
    p_atom = memo(alt(p_int, p_word))
    p_expr = memo(fmap(expr, star(p_atom)))

    p_word_sig_param = fmapseq(lambda a,_,b,c: (a,b,c),
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

    p_word_sig = fmapseq(lambda a,p,_,b,c: word_sig(a,p,b,c),
        p_name,
        p_word_sig_params,
        test(token.is_colon),
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
        parens(starsep(p_comma, p_name)),
        pure(lambda: []),
    )

    p_word_def = fmapseq(lambda a,p,_,b: word_def(a,p,b),
        p_name,
        p_word_def_params,
        test(token.is_equal),
        p_expr,
    )

    p_assertion = fmapseq(lambda a,c,b: assertion(c.lineno, a,b),
        p_expr,
        test(token.is_equal2),
        p_expr,
    )

    p_decl = fmapseq(lambda a,b: a,
        alt(
            p_word_sig,
            p_word_def,
            p_assertion,
            p_expr
        ),
        p_line
    )

    p_file = star(p_decl)
    r = p_file(0)
    if not r:
        raise SyntaxError("parse error at unknown line")
    v,i = r
    if i < len(tokens):
        raise SyntaxError("parse error at line %d" % tokens[i].lineno)
    return v

def parse(code):
    '''Parse code or lines of code.

    >>> [str(x) for x in parse('foo bar')]
    ['foo bar']
    >>> [str(x) for x in parse('10 20 +')]
    ['10 20 +']
    >>> parse('10')
    [expr([intlit(token('10', 1))])]
    >>> parse('foo(bar)')
    [expr([word(token('foo', 1), [expr([word(token('bar', 1), [])])])])]
    >>> parse('foo : bar')
    [word_sig(token('foo', 1), [], expr([]), expr([word(token('bar', 1), [])]))]
    >>> parse('foo : bar -- baz')
    [word_sig(token('foo', 1), [], expr([word(token('bar', 1), [])]), expr([word(token('baz', 1), [])]))]
    >>> parse('foo(f : bar) : baz')
    [word_sig(token('foo', 1), [(token('f', 1), expr([]), expr([word(token('bar', 1), [])]))], expr([]), expr([word(token('baz', 1), [])]))]
    >>> parse('foo = bar')
    [word_def(token('foo', 1), [], expr([word(token('bar', 1), [])]))]
    >>> parse('foo == bar')
    [assertion(1, expr([word(token('foo', 1), [])]), expr([word(token('bar', 1), [])]))]
    '''
    return parsetoks(tokenize(code))

##############################################################################
################################# SYNTAX #####################################
##############################################################################

class intlit:
    def __init__(self, token):
        self.token = token
        self.value = int(token.code)

    def __repr__(self):
        return 'intlit(%r)' % self.token

    def __str__(self):
        return str(self.value)

    def elab(self, env):
        return env.elab_push_int(self.value)

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

class word_sig:
    def __init__(self, name, params, dom, cod):
        self.name = name
        self.params = params
        self.dom = dom
        self.cod = cod

    def __repr__(self):
        return 'word_sig(%r, %r, %r, %r)' % (self.name, self.params, self.dom, self.cod)

    def decl(self, mod):
        return mod.decl_word_sig(self.name.code, self.params, self.dom, self.cod)

class word_def:
    def __init__(self, name, params, body):
        self.name   = name
        self.params = params
        self.body   = body

    def __repr__(self):
        return 'word_def(%r, %r, %r)' % (self.name, self.params, self.body)

    def decl(self, mod):
        return mod.decl_word_def(self.name.code, [p.code for p in self.params], self.body)

class assertion:
    def __init__(self, lineno, lhs, rhs):
        self.lineno = lineno
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self):
        return 'assertion(%r, %r, %r)' % (self.lineno, self.lhs, self.rhs)

    def decl(self, mod):
        return mod.decl_assertion(self.lineno, self.lhs, self.rhs)

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
            raise TypeError("Failed to unify %s and %s." % (self, tcon(other_name, *other_args)))

    def unify_tvar(self, other_name, sub):
        return tvar(other_name).unify_tcon(self.name, self.args, sub)

    def unify_tpack(self, other_rest, other_args, sub):
        if other_args == [] and other_rest is not None:
            return other_rest.unify(self, sub)
        raise TypeError("Failed to unify %s and %s" % (self, tpack(other_rest, *other_args)))

class tvar:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'tvar(%r)' % self.name

    def __str__(self):
        return self.name

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

    def unify_tpack(self, other_rest, other_args, sub):
        if self.name in sub:
            return sub[self.name].unify_tpack(other_rest, other_args, sub)
        elif other_args == [] and other_rest is not None:
            return self.unify(other_rest, sub)
        else:
            newself = tpack(other_rest, *other_args).subst(sub)
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
    >>> tpack(None, tvar('x'))
    tpack(None, tvar('x'))
    >>> tpack(tvar('x'), tvar('y'))
    tpack(tvar('x'), tvar('y'))
    >>> tpack(tvar('x'), tvar('y')).unify(tpack(tvar('z'), tcon('Int', []), tcon('Str', [])), {})
    tpack(tvar('z'), tcon('Int'), tcon('Str'))
    >>> tpack(tvar('x'), tvar('y')).unify(tpack(tvar('y'), tcon('Int', []), tcon('Str', [])), {})
    tpack(tcon('Str'), tcon('Int'), tcon('Str'))
    """
    def __init__ (self, rest=None, *args):
        while isinstance(rest, tpack):
            args = rest.args + list(args)
            rest = rest.rest
        self.rest = rest
        self.args = list(args)

    def __repr__ (self):
        if self.rest == None and self.args == []:
            return 'tpack()'
        else:
            return 'tpack(%r, %s)' % (self.rest, ', '.join(repr(a) for a in self.args))

    def __str__ (self):
        if self.rest is None:
            return '[ %s ]' % ' '.join(str(a) for a in self.args)
        else:
            return '[ *%s %s ]' % (self.rest, ' '.join(str(a) for a in self.args))

    def rigidify(self):
        if self.rest is None:
            return tpack(None, *[a.rigidify() for a in self.args])
        else:
            return tpack(self.rest.rigidify(), *[a.rigidify() for a in self.args])

    def freshen(self, prefix):
        if self.rest is None:
            return tpack(None, *[a.freshen(prefix) for a in self.args])
        else:
            return tpack(self.rest.freshen(prefix), *[a.freshen(prefix) for a in self.args])

    def has_var(self, name):
        if self.rest is None:
            return any(a.has_var(name) for a in self.args)
        else:
            return self.rest.has_var(name) or any(a.has_var(name) for a in self.args)

    def subst(self, sub):
        if self.rest is None:
            return tpack(None, *[a.subst(sub) for a in self.args])
        else:
            return tpack(self.rest.subst(sub), *[a.subst(sub) for a in self.args])

    def unify(self, other, sub):
        return other.unify_tpack(self.rest, self.args, sub)

    def unify_tcon(self, other_name, other_args, sub):
        if self.args == [] and self.rest is not None:
            return self.rest.unify_tcon(other_name, other_args, sub)
        raise TypeError("Failed to unify %s and %s." % (self, tcon(other_name, other_args)))

    def unify_tvar(self, other_name, sub):
        return tvar(other_name).unify_tpack(self.rest, self.args, sub)

    def unify_tpack(self, other_rest, other_args, sub):
        if len(self.args) < len(other_args):
            return tpack(other_rest, *other_args).unify_tpack(self.rest, self.args, sub)
        elif len(self.args) > len(other_args):
            if other_rest is None:
                raise TypeError("Failed to unify %s and %s" % (self, tpack(other_rest, *other_args)))
            n = len(self.args) - len(other_args)
            (largs, rargs) = (self.args[:n], self.args[n:])
            newrest = other_rest.unify_tpack(self.rest, largs, sub)
            newargs = []
            for (rarg, oarg) in zip(rargs, other_args):
                newargs.append(rarg.unify(oarg, sub))
            return tpack(newrest, *newargs).subst(sub)
        else:
            npack = tpack()
            srest = self.rest if self.rest else npack
            orest = other_rest if other_rest else npack
            newrest = srest if srest is orest else srest.unify(orest, sub)
            newargs = []
            for (rarg, oarg) in zip(self.args, other_args):
                newargs.append(rarg.unify(oarg, sub))
            return tpack(newrest, *newargs).subst(sub)


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
        self.types = builtin_types.copy()
        self.word_sigs = builtin_word_sigs.copy()
        self.word_defs = builtin_word_defs.copy()
        self.assertions = []

    def has_type(self, name):
        return name in self.types

    def get_type(self, name, args):
        if name not in self.types:
            raise TypeError("Type %s not defined." % name)
        return self.types[name] (self, args)

    def get_word_sig (self, name):
        if name not in self.word_sigs:
            raise NameError("Word %s is not declared." % name)
        return self.word_sigs[name]

    def get_word_def (self, name):
        if name not in self.word_defs:
            raise NameError("Word %s is not defined." % name)
        return self.word_defs[name]

    def decl_word_sig (self, name, params, dom, cod):
        if name in self.word_sigs:
            raise TypeError("Word %s is declared twice." % name)

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
        self.word_sigs[name] = (ps, domte.to_tpack(), codte.to_tpack())

    def decl_word_def (self, name, params, body):
        if name not in self.word_sigs:
            raise TypeError("Word %s is defined without type signature." % name)

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
        cod.rigidify().unify(cod2, {}) # if this passes then we are golden
        self.word_defs[name] = func

    def decl_assertion (self, lineno, lhs, rhs):
        orig = tpack(fresh_var())

        elab1 = word_elaborator(self, orig)
        elab2 = word_elaborator(self, orig)

        lhsf = lhs.elab(elab1)
        rhsf = rhs.elab(elab2)

        cosub = {}
        elab1.dom.unify(elab2.dom, cosub)
        orig = orig.subst(elab1.sub).unify(orig.subst(elab2.sub), cosub)
        self.assertions.append((lineno, orig, lhsf, rhsf))

    def decl_expr (self, expr):
        raise SyntaxError("Bare expression not supported.")

    def check_assertions(self):
        for a in self.assertions:
            self.check_assertion(a)

    def check_assertion(self, assn):
        (lineno, dom, f0, f1) = assn
        tries = 100 if len(dom.args) else 1
        for try_number in range(tries):
            e0 = env(self)
            e1 = env(self)
            vs = []
            for d in dom.args:
                v = self.arbitrary(d)
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

    def arbitrary(self, t):
        if isinstance(t, tvar):
            return random.randint(-10, 10)
        elif isinstance(t, tcon):
            if t.name == 'Int' and t.args == []:
                return random.randint(-10, 10)

        raise TypeError("Assertion requires input of type %s -- don't know how to generate." % t)

class type_elaborator:
    def __init__(self, mod):
        self.mod = mod
        self.rest = None
        self.args = []

    def to_tpack(self):
        return tpack(self.rest, *self.args)

    def elab_push_int(self, value):
        raise TypeError("Expected a type but got an int.")

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
        self.dom = tpack(self.dom, tcon('Int', []))
        return lambda p: p.push(value)

    def elab_word(self, name, args):
        if name in self.loc:
            (nargi, dom, cod) = self.loc[name]

            wargs = []
            def mkfn(fs):
                def fn(p, *nargs):
                    p.copush(nargs[nargi])
                return fn

        else:
            (wargs, dom, cod) = self.mod.get_word_sig(name)

            def mkfn(fs):
                def fn(p, *nargs):
                    def pushedfn(e):
                        d = e.mod.get_word_def(name)
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
            dom = tpack(ovar, *dom.args)
            cod = tpack(ovar, *cod.args)

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
                p.copush(lambda e: fn(e, *args))
            for fn in fns[::-1]:
                g(fn)
        return f

class env:
    def __init__(self, mod):
        self.mod = mod
        self.stack  = []
        self.rstack = []

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


##############################################################################
################################ BUILTINS ####################################
##############################################################################

def type0 (t):
    def f(mod, args):
        if len(args) > 0:
            raise TypeError("%s takes no arguments." % name)
        return t
    return f

def mktpack (mod, args):
    if len(args) != 0:
        raise TypeError("Pack takes one argument.")
    e = type_elaborator(mod)
    args[0].elab(e)
    return e.to_tpack()

tint  = tcon('Int')
tstr  = tcon('Str')
tbool = tcon('Bool')

builtin_types = {
    'Int':  type0(tint),
    'Str':  type0(tstr),
    'Bool': type0(tbool),
    'Pack': mktpack,
}

def word2 (f):
    def w(e):
        b = e.pop()
        a = e.pop()
        e.push(f(a,b))
    return w

builtin_word_sigs = {

    # basic
    'dup':  ([], tpack(None, tvar('b')), tpack(None, tvar('b'), tvar('b'))),
    'drop': ([], tpack(None, tvar('b')), tpack(None)),
    'swap': ([], tpack(None, tvar('b'), tvar('c')), tpack(None, tvar('c'), tvar('b'))),
    'id':   ([], tpack(None), tpack(None)),
    'dip':  ([(tpack(tvar('a')), tpack(tvar('b')))],
            tpack(tvar('a'), tvar('c')),
            tpack(tvar('b'), tvar('c'))),

    # bool
    'true':  ([], tpack(), tpack(None, tbool)),
    'false': ([], tpack(), tpack(None, tbool)),
    'if': ([(tpack(tvar('a')), tpack(tvar('b')))
           ,(tpack(tvar('a')), tpack(tvar('b')))],
           tpack(tvar('a'), tbool), tpack(tvar('b'))),

    # int
    '+': ([], tpack(None, tint, tint), tpack(None, tint)),
    '-': ([], tpack(None, tint, tint), tpack(None, tint)),
    '*': ([], tpack(None, tint, tint), tpack(None, tint)),
    '/': ([], tpack(None, tint, tint), tpack(None, tint)),
    '%': ([], tpack(None, tint, tint), tpack(None, tint)),
    '<': ([], tpack(None, tint, tint), tpack(None, tbool)),

}

builtin_word_defs = {
    # basic
    'dup':  env.dup,
    'drop': env.drop,
    'swap': env.swap,
    'id':   (lambda env: env),
    'dip':  env.dip,

    # bool
    'true':  (lambda env: env.push(True)),
    'false': (lambda env: env.push(False)),
    'if':   env.w_if,

    # int
    '+': word2(lambda a,b: a + b),
    '-': word2(lambda a,b: a - b),
    '*': word2(lambda a,b: a * b),
    '%': word2(lambda a,b: a % b),
    '/': word2(lambda a,b: a // b),
    '<': word2(lambda a,b: a < b),

}



##############################################################

if __name__ == '__main__':
    main()


