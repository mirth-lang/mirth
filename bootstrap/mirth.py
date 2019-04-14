#!/usr/bin/env python3

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


'''
This is a minimalistic mirth interpreter. It is not designed to be
correct or complete -- it is designed to be a small first step to
bootstrapping the compiler. Never use this without copious tests.

Usage:

    python3 mirth.py


'''

def main():
    import doctest
    doctest.testmod()

    m = module()
    e = env(m)
    l = word_elaborator(m, [])

    try:
        keep_going = True
        while keep_going:
            code = input(">>> ")
            try:
                decls = parse(code)
                for decl in decls:
                    if isinstance(decl, expr):
                        f = decl.elab(l)
                        f(e)
                        e.run(timeout=1000000)
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

    except KeyboardInterrupt:
        pass


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
reserved = {'\n', '(', ')', ',', ':', '--', '=', '==', 'type', 'data', 'end'}

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
    >>> parse('foo = bar')
    [word_def(token('foo', 1), expr([word(token('bar', 1), [])]))]
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
        self.word_sigs = {}
        self.word_defs = {}
        self.assertions = []

    def get_type(self, name, args):
        if len(args) > 0:
            raise TypeError("Type arguments not yet implemented.")
        if name not in self.types:
            raise TypeError("Type %s not defined." % name)
        return self.types[name]

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
        if name not in self.word_sigs:
            raise TypeError("Word %s is defined without type signature." % name)
        if name in self.word_defs:
            raise TypeError("Word %s is defined twice." % name)

        (dom, cod) = self.word_sigs[name]
        elab = word_elaborator(self, dom)
        func = body.elab(elab)
        cod2 = elab.dom
        if cod != cod2: # placeholder
            raise TypeError(
                "Word %s has mismatched output type. Expected ( %s ) but got ( %s )"
                    % (name, ' '.join(cod), ' '.join(cod2))
            )
        self.word_defs[name] = func

    def decl_assertion (self, lhs, rhs):
        self.assertions.append((lhs, rhs))

    def decl_expr (self, expr):
        raise SyntaxError("Bare expression not yet supported.")

class type_elaborator:
    def __init__(self, module):
        self.module = module

    def elab_push_int(self, value):
        raise TypeError("Expected a type but got an int.")

    def elab_word(self, name, args):
        return [self.module.get_type(name, args)]

    def elab_expr(self, atoms):
        ts = []
        for atom in atoms:
            ts.extend(atom.elab(self))
        return ts

class word_elaborator:
    def __init__(self, module, dom):
        self.module = module
        self.dom = list(dom)[:]

    def elab_push_int(self, value):
        self.dom.append('Int')
        return lambda p: p.push(value)

    def elab_word(self, name, args):
        if len(args):
            raise SyntaxError("Word arguments not yet implemented.")
        (dom, cod) = self.module.get_word_sig(name)
    
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
            for fn in fns:
                fn(p)
        return f

class env:
    def __init__(self, module):
        self.module = module
        self.stack  = []
        self.rstack = []

    def push(self, v):
        self.stack.append(v)

    def copush(self, r):
        self.rstack.append(r)

    def step(self):
        if self.rstack:
            w = self.rstack.pop()
            if callable(w):
                w(self)
            elif isinstance(w, str):
                self.module.get_word_def(w) (self)
            else:
                raise TypeError("Unknown item on return stack: %r" % w)
            return True
        return False

    def run(self, timeout=100000):
        r = 0
        while self.step():
            r += 1
            if r > timeout:
                raise ValueError("Ran out of time (infinite loop?).")

    def show_stack(self):
        print(' '.join(repr(s) for s in self.stack))


##############################################################

if __name__ == '__main__':
    main()


