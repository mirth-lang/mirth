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

'''

def main():
    import sys

    if len(sys.argv) == 1:
        repl()
    elif len(sys.argv) == 2:
        if sys.argv[1] == '--doctest':
            import doctest
            doctest.testmod()
        else:
            interpret(sys.argv[1])
    else:
        print("USAGE: %s [FILE]" % sys.argv[0])
        sys.exit(1)

def interpret(path):
    with open(path) as fp:
        decls = parse(fp)

    m = module()
    for d in decls:
        d.decl(m)

    m.check_assertions()

def repl():
    # REPL banner
    print()
    print('Mirth Bootstrap Interpreter v0.0.0: CAVEAT USOR.')
    print()
    print('Thank you to our patrons on http://patreon.com/mirth_lang')
    print('And a special thanks to our Super Patron, Benjohn.')
    print()

    m = module()
    e = env(m)
    l = word_elaborator(m, [])

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

    p_prim_def = fmapseq(lambda a,_,b: prim_def(a,b),
        p_name,
        test(token.is_coloneq),
        p_expr
    )

    p_word_sig = fmapseq(lambda a,_,b,c: word_sig(a,b,c),
        p_name,
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

    p_word_def = fmapseq(lambda a,_,b: word_def(a,b),
        p_name,
        test(token.is_equal),
        p_expr,
    )

    p_assertion = fmapseq(lambda a,_,b: assertion(a,b),
        p_expr,
        test(token.is_equal2),
        p_expr,
    )

    p_decl = fmapseq(lambda a,b: a,
        alt(
            p_prim_def,
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
    [word_sig(token('foo', 1), expr([]), expr([word(token('bar', 1), [])]))]
    >>> parse('foo : bar -- baz')
    [word_sig(token('foo', 1), expr([word(token('bar', 1), [])]), expr([word(token('baz', 1), [])]))]
    >>> parse('foo = bar')
    [word_def(token('foo', 1), expr([word(token('bar', 1), [])]))]
    >>> parse('foo := bar')
    [prim_def(token('foo', 1), expr([word(token('bar', 1), [])]))]
    >>> parse('foo == bar')
    [assertion(expr([word(token('foo', 1), [])]), expr([word(token('bar', 1), [])]))]
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
    def __init__(self, name, dom, cod):
        self.name = name
        self.dom = dom
        self.cod = cod

    def __repr__(self):
        return 'word_sig(%r, %r, %r)' % (self.name, self.dom, self.cod)

    def decl(self, mod):
        return mod.decl_word_sig(self.name.code, self.dom, self.cod)

class word_def:
    def __init__(self, name, body):
        self.name = name
        self.body = body

    def __repr__(self):
        return 'word_def(%r, %r)' % (self.name, self.body)

    def decl(self, mod):
        return mod.decl_word_def(self.name.code, self.body)

class prim_def:
    def __init__(self, name, body):
        self.name = name
        self.body = body

    def __repr__(self):
        return 'prim_def(%r, %r)' % (self.name, self.body)

    def decl(self, mod):
        return mod.decl_prim_def(self.name.code, self.body)

class assertion:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def __repr__(self):
        return 'assertion(%r, %r)' % (self.lhs, self.rhs)

    def decl(self, mod):
        return mod.decl_assertion(self.lhs, self.rhs)


##############################################################################
############################### ELABORATOR ###################################
##############################################################################

class module:
    def __init__(self):
        self.types = {'Int': 'Int'} # placeholder
        self.prims = builtin_prims.copy()
        self.word_sigs = builtin_word_sigs.copy()
        self.word_defs = builtin_word_defs.copy()
        self.assertions = []

    def get_type(self, name, args):
        if len(args) > 0:
            raise TypeError("Type arguments not yet implemented.")
        if name not in self.types:
            raise TypeError("Type %s not defined." % name)
        return self.types[name]

    def has_prim (self, name):
        return name in self.prims

    def get_prim (self, name):
        if name not in self.prims:
            raise NameError("Primitive %s is not defined." % name)
        return self.prims[name]

    def get_word_sig (self, name):
        if name not in self.word_sigs:
            raise NameError("Word %s is not declared." % name)
        return self.word_sigs[name]

    def get_word_def (self, name):
        if name not in self.word_defs:
            raise NameError("Word %s is not defined." % name)
        return self.word_defs[name]

    def decl_word_sig (self, name, dom, cod):
        if name in self.word_sigs:
            raise TypeError("Word %s is declared twice." % name)
        domts = dom.elab(type_elaborator(self))
        codts = cod.elab(type_elaborator(self))
        self.word_sigs[name] = (domts, codts)

    def decl_word_def (self, name, body):
        if name in self.word_defs or name in self.prims:
            raise TypeError("Word %s is defined twice." % name)
        if name not in self.word_sigs:
            raise TypeError("Word %s is defined without type signature." % name)

        (dom, cod) = self.word_sigs[name]
        elab = word_elaborator(self, dom)
        func = body.elab(elab)
        cod2 = elab.dom
        if cod != cod2: # placeholder
            raise TypeError(
                "Word %s has mismatched output type. Expected [%s] but got [%s]."
                    % (name, ' '.join(cod), ' '.join(cod2))
            )
        self.word_defs[name] = func

    def decl_prim_def (self, name, body):
        if name in self.word_defs or name in self.prims:
            raise TypeError("Prim %s is defined twice." % name)
        if name in self.word_sigs:
            raise TypeError("Prim %s has type signature." % name)

        def f(e,args):
            if len(args) > 0:
                raise TypeError("Prim %s takes no arguments." % name)
            return body.elab(e)

        self.prims[name] = f

    def decl_assertion (self, lhs, rhs):
        elab1 = word_elaborator(self, [])
        elab2 = word_elaborator(self, [])

        lhsf = lhs.elab(elab1)
        rhsf = rhs.elab(elab2)

        if elab1.dom != elab2.dom:
            raise TypeError(
               "Assertion type mismatch: LHS has [%s], RHS has [%s]."
                % (' '.join(elab1.dom), ' '.join(elab2.dom))
            )

        self.assertions.append((lhsf, rhsf))

    def decl_expr (self, expr):
        raise SyntaxError("Bare expression not yet supported.")


    def check_assertions(self):
        for a in self.assertions:
            self.check_assertion(a)

    def check_assertion (self, assn):
        (f0, f1) = assn
        e0 = env(self)
        e1 = env(self)
        f0(e0)
        f1(e1)
        e0.run()
        e1.run()
        if e0.stack != e1.stack:
            raise ValueError("Assertion failed: LHS = [%s], RHS = [%s]."
                % ( ' '.join(map(repr, e0.stack))
                  , ' '.join(map(repr, e1.stack)) ))

class type_elaborator:
    def __init__(self, mod):
        self.mod = mod

    def elab_push_int(self, value):
        raise TypeError("Expected a type but got an int.")

    def elab_word(self, name, args):
        return [self.mod.get_type(name, args)]

    def elab_expr(self, atoms):
        ts = []
        for atom in atoms:
            ts.extend(atom.elab(self))
        return ts


class word_elaborator:
    def __init__(self, mod, dom):
        self.mod = mod
        self.dom = list(dom)[:]

    def elab_push_int(self, value):
        self.dom.append('Int')
        return lambda p: p.push(value)

    def elab_word(self, name, args):
        if self.mod.has_prim(name):
            return self.mod.get_prim(name) (self, args)

        if len(args):
            raise SyntaxError("Word arguments not yet implemented.")
        (dom, cod) = self.mod.get_word_sig(name)

        if len(dom) == 0:
            dom_extra, dom_used = self.dom, []
        else:
            dom_extra, dom_used = self.dom[:-len(dom)], self.dom[-len(dom):]

        if dom_used != dom:
            raise TypeError(
                "Type mismatch for usage of word %s: expcted %s but got %s"
                % (name, dom, self.dom)
            )
        self.dom = dom_extra + cod # placeholder
        return lambda p: p.copush(name)

    def elab_expr(self, atoms):
        fns = []
        for atom in atoms:
            fns.append(atom.elab(self))
        def f(p):
            for fn in fns[::-1]:
                p.copush(fn)
        return f

    # prims

    def elab_id(self, *args):
        if len(args) != 0:
            raise TypeError("Prim id takes no arguments.")
        return lambda env: None

    def elab_dup (self, *args):
        if len(args) != 0:
            raise TypeError("Prim dup takes no arguments.")
        if len(self.dom) <= 0:
            raise TypeError("Can't dup on empty stack.")
        self.dom.append(self.dom[-1])
        return lambda env: env.dup()

    def elab_drop (self, *args):
        if len(args) != 0:
            raise TypeError("Prim drop takes no arguments.")
        if len(self.dom) <= 0:
            raise TypeError("Can't drop on empty stack.")
        self.dom.pop()
        return lambda env: env.drop()

    def elab_dip (self, *args):
        if len(args) != 1:
            raise TypeError("Prim dip takes 1 argument.")
        if len(self.dom) <= 0:
            raise TypeError("Can't drop on empty stack.")

        body = args[0]
        t = self.dom.pop()
        f = body.elab(self)
        self.dom.append(t)
        return lambda env: env.dip(f)

    def elab_swap (self, *args):
        if len(args) != 0:
            raise TypeError("Prim swap takes no arguments.")
        if len(self.dom) <= 1:
            raise TypeError("Can't swap on stack with less than 2 elements.")
        b = self.dom.pop()
        a = self.dom.pop()
        self.dom.append(b)
        self.dom.append(a)
        return lambda env: env.swap()

builtin_prims = {
    'id':   lambda e, args: e.elab_id   (*args),
    'dup':  lambda e, args: e.elab_dup  (*args),
    'drop': lambda e, args: e.elab_drop (*args),
    'swap': lambda e, args: e.elab_swap (*args),
    'dip':  lambda e, args: e.elab_dip  (*args),
}



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

    def step(self):
        if self.rstack:
            w = self.copop()
            if callable(w):
                w(self)
            elif isinstance(w, str):
                self.mod.get_word_def(w) (self)
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

def word2 (f):
    def w(env):
        b = env.pop()
        a = env.pop()
        env.push(f(a,b))
    return w


builtin_word_sigs = {
    '+': (['Int', 'Int'], ['Int']),
    '-': (['Int', 'Int'], ['Int']),
    '*': (['Int', 'Int'], ['Int']),
    '%': (['Int', 'Int'], ['Int']),
    '/': (['Int', 'Int'], ['Int']),
}

builtin_word_defs = {
    '+': word2(lambda a,b: a + b),
    '-': word2(lambda a,b: a - b),
    '*': word2(lambda a,b: a * b),
    '%': word2(lambda a,b: a % b),
    '/': word2(lambda a,b: a // b),
}



##############################################################

if __name__ == '__main__':
    main()


