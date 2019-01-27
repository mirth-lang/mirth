#!/usr/bin/env python3

############################### LICENSE ###############################
# This Source Code Form is subject to the terms of the Mozilla Public #
# License, v. 2.0. If a copy of the MPL was not distributed with this #
# file, You can obtain one at http://mozilla.org/MPL/2.0/.            #
#######################################################################

import re
import copy

def exception(fn):
    r"""Get thrown exception from function and return it.
    Returns None if there is no exception.

    >>> exception(lambda: 0)
    >>> exception(lambda: 10 / 0)
    ZeroDivisionError('division by zero',)
    """
    try:
        fn()
    except Exception as e:
        return e

def assert_type(val, ty, name=None):
    '''Assert value has given type before returning value.

    >>> assert_type(1, int)
    1
    >>> assert_type('foo', str)
    'foo'
    >>> exception(lambda: assert_type([], bool))
    TypeError('expected bool got []',)
    >>> exception(lambda: assert_type(1, str, "filename"))
    TypeError('expected str got 1 for filename',)
    '''
    if not isinstance(val, ty):
        msg = 'expected %s got %r' % (ty.__name__, val)
        if name:
            raise TypeError('%s for %s' % (msg, name))
        else:
            raise TypeError(msg)
    return val

def assert_type_or_none(val, ty, name=None):
    if val is None:
        return None
    else:
        return assert_type(val, ty, name)

def unescape_str (s):
    r"""Remove escape from captured string.

    >>> unescape_str('"foo"')
    'foo'
    >>> unescape_str("'bar'")
    'bar'
    >>> exception(lambda: unescape_str("foo"))
    ValueError('Unknown string delimiter: f',)
    >>> exception(lambda: unescape_str("'foo"))
    ValueError('Unknown string delimiter: o',)
    >>> exception(lambda: unescape_str("'foo\""))
    ValueError('Mismatched string delimiters: \' and "',)
    >>> unescape_str(r'"helwo\\lorld"')
    'helwo\\lorld'
    >>> unescape_str(r'"\n\t\r\'\""')
    '\n\t\r\'"'
    >>> exception(lambda: unescape_str(r'"\l"'))
    SyntaxError('Unknown escape sequence: \\l',)
    >>> exception(lambda: unescape_str(r'"\"'))
    ValueError('Unexpected escape at end of string.',)
    """
    if s[0] not in ['"', "'"]:
        raise ValueError("Unknown string delimiter: %c" % s[0])
    if s[-1] not in ['"', "'"]:
        raise ValueError("Unknown string delimiter: %c" % s[-1])
    if s[0] != s[-1]:
        raise ValueError("Mismatched string delimiters: %c and %c"
                        % (s[0], s[-1]))

    s = s[1:-1]
    r = []
    escaped = False
    for c in s:
        if escaped:
            escaped = False
            if c in {'\\', '"', "'"}:
                r.append(c)
            elif c == 'n':
                r.append('\n')
            elif c == 't':
                r.append('\t')
            elif c == 'r':
                r.append('\r')
            else:
                raise SyntaxError("Unknown escape sequence: \\%c" % c)
        elif c == '\\':
            escaped = True
        else:
            r.append(c)
    if escaped:
        raise ValueError("Unexpected escape at end of string.")

    return ''.join(r)


# rule types
OPEN   = 'OPEN'
CLOSE  = 'CLOSE'
ATOMIC = 'ATOMIC'
IGNORE = 'IGNORE'

# token names
LPAREN = 'LPAREN'
RPAREN = 'RPAREN'
LSQUARE = 'LSQUARE'
RSQUARE = 'RSQUARE'
LCURLY = 'LCURLY'
RCURLY = 'RCURLY'
NEWLINE = 'NEWLINE'
COMMENT = 'COMMENT'
COMMA = 'COMMA'
STR = 'STR'
INT = 'INT'
WORD = 'WORD'
SPACE = 'SPACE'


class Rule:
    KINDS = {OPEN, CLOSE, ATOMIC, IGNORE}

    def __init__(self, kind, name, regex, to_value=None):
        if kind not in Rule.KINDS:
            raise ValueError("%s nat a valid rule kind" % kind)
        self.kind = assert_type(kind, str, 'kind')
        self.name = assert_type(name, str, 'name')
        self.regex = re.compile(assert_type(regex, str, 'regex'))
        self.to_value = to_value if to_value else lambda x: x

base_rules = [
    Rule(OPEN,   LPAREN,  r'\('),
    Rule(CLOSE,  RPAREN,  r'\)'),
    Rule(OPEN,   LSQUARE, r'\['),
    Rule(CLOSE,  RSQUARE, r'\]'),
    Rule(OPEN,   LCURLY,  r'\{'),
    Rule(CLOSE,  RCURLY,  r'\}'),
    Rule(ATOMIC, NEWLINE, r'\n'),
    Rule(ATOMIC, COMMENT, r'#[^\r\n]*[\r\n]*'),
    Rule(ATOMIC, COMMA,   r','),
    Rule(ATOMIC, STR,     r'"([^\\"]|\\.)*"', to_value=unescape_str),
    Rule(ATOMIC, STR,     r"'([^\\']|\\.)*'", to_value=unescape_str),
    Rule(ATOMIC, INT,     r'[\+\-]?\d+', to_value=int),
    Rule(ATOMIC, INT,     r'0[xX][0-9a-zA-Z]+', to_value=lambda x: int(x,16)),
    Rule(ATOMIC, WORD,    r'[^\s\(\)\[\]\{\},]+'),
    Rule(IGNORE, SPACE,   r'[ \t\r\f\v]+'),
]

class Pure:
    def can_dup (self): return True
    def can_drop (self): return True

class Loc (Pure):
    def __init__ (self, path=None, row=1, col=1):
        self.path = path
        self.row = row
        self.col = col

    def __str__ (loc):
        if loc.path:
            return '%s:%d:%d' % (loc.path, loc.row, loc.col)
        else:
            return '%d:%d' % (loc.row, loc.col)

    def __repr__ (loc):
        return 'Loc(%r, %r, %r)' % (loc.path, loc.row, loc.col)

    def prefix (loc):
        if loc:
            return '%s: ' % loc_show(loc)
        else:
            return ''

    def next(self, text, **etc):
        TABSTOP = etc.get('TABSTOP', 8)
        row, col = self.row, self.col
        for c in text:
            if c == '\n':
                row += 1
                col  = 1
            elif c == '\t':
                col += TABSTOP - col % TABSTOP
            else:
                col += 1
        return Loc(self.path, row, col)

    def decons(self, env):
        env.push(self.path, self.row, self.col)

class Token (Pure):
    def __init__ (self, kind, name, text, value, loc):
        self.kind = assert_type(kind, str, 'kind')
        if kind not in Rule.KINDS:
            raise ValueError("%s nat a valid rule kind" % kind)
        self.name = assert_type(name, str, 'name')
        self.text = assert_type(text, str, 'text')
        self.value = value
        self.loc = assert_type(loc, Loc, 'text')

    def __repr__ (self):
        return 'Token(%r, %r, %r, %r, %r)' % (
                self.kind, self.name, self.text, self.value, self.loc
                )


        def __len__ (self):
            return len(self.text)

    def next_loc(self, **etc):
        return self.loc.next(self.text, **etc)

    def can_dup (self):
        return can_dup(self.value)

    def can_drop (self):
        return can_drop(self.value)

    def decons (self, env):
        env.push(self.kind, self.name, self.text, self.value, self.loc)



def next_token (code, loc=Loc(None,1,1), rules=base_rules):
    r'''Get the next token based on rules.
    If no rule matches / end of file, returns None.
    If multiple rules match, returns the longest match.
    If multiple rules match with the same length, returns the first such match
    (the rules are ordered).

    >>> next_token('')
    >>> next_token('123')
    (Token('ATOMIC', 'INT', '123', 123, Loc(None, 1, 1)), '', Loc(None, 1, 4))
    >>> next_token('0xdeadbeef ', Loc(None,100,1))
    (Token('ATOMIC', 'INT', '0xdeadbeef', 3735928559, Loc(None, 100, 1)), ' ', Loc(None, 100, 11))
    >>> next_token('1st', Loc(None,10,20))
    (Token('ATOMIC', 'WORD', '1st', '1st', Loc(None, 10, 20)), '', Loc(None, 10, 23))
    >>> exception(lambda: next_token('"\l"', Loc('jeff.txt',2,3)))
    SyntaxError('jeff.txt:2:3: Unknown escape sequence: \\l',)
    >>> next_token('foo(bar baz)', Loc(None,1,1))
    (Token('ATOMIC', 'WORD', 'foo', 'foo', Loc(None, 1, 1)), '(bar baz)', Loc(None, 1, 4))
    '''
    result = None
    best_len = 0
    for rule in rules:
        match = rule.regex.match(code)
        if match:
            text = match.group()
            if len(text) > best_len:
                best_len = len(text)
                try:
                    value = rule.to_value(text)
                except SyntaxError as e:
                    raise SyntaxError("%s: %s" % (loc, e.msg))
                result = Token(rule.kind, rule.name, text, value, loc)

    if result:
        return result, code[best_len:], result.next_loc()
    if code:
        short_code = code if len(code) < 8 else code[:5] + '...'
        raise SyntaxError("%s: expected token, got %r" % (loc, short_code))


def tokenize(code, loc=Loc(None,1,1), rules=base_rules):
    r'''Tokenize source all at once according to rules. This ignores
    lexer macros. As such, it is not suitable for modules that
    introduce or import lexer macros (if such a thing is to exist).

    >>> tokenize('')
    []
    >>> tokenize('foo')
    [Token('ATOMIC', 'WORD', 'foo', 'foo', Loc(None, 1, 1))]
    >>> tokenize('foo bar')
    [Token('ATOMIC', 'WORD', 'foo', 'foo', Loc(None, 1, 1)), Token('ATOMIC', 'WORD', 'bar', 'bar', Loc(None, 1, 5))]
    >>> tokenize('foo(1 2 \n +)')
    [Token('ATOMIC', 'WORD', 'foo', 'foo', Loc(None, 1, 1)), Token('OPEN', 'LPAREN', '(', '(', Loc(None, 1, 4)), Token('ATOMIC', 'INT', '1', 1, Loc(None, 1, 5)), Token('ATOMIC', 'INT', '2', 2, Loc(None, 1, 7)), Token('ATOMIC', 'NEWLINE', '\n', '\n', Loc(None, 1, 9)), Token('ATOMIC', 'WORD', '+', '+', Loc(None, 2, 2)), Token('CLOSE', 'RPAREN', ')', ')', Loc(None, 2, 3))]
    '''
    toks = []
    match = next_token(code, loc)
    while match:
        tok, code, loc = match
        if tok.kind != IGNORE:
            toks.append(tok)
        match = next_token(code, loc)
    return toks

def parse_tokens (tokens):
    r'''Parse tokens into a syntax tree.

    >>> parse_tokens(tokenize(''))
    Expr([])
    >>> parse_tokens(tokenize('10 20 30'))
    Expr([LitInt(10), LitInt(20), LitInt(30)])
    >>> parse_tokens(tokenize('hello'))
    Expr([Atom('hello', [])])
    >>> parse_tokens(tokenize('foo bar'))
    Expr([Atom('foo', []), Atom('bar', [])])
    >>> parse_tokens(tokenize('foo(bar,baz)'))
    Expr([Atom('foo', [Expr([Atom('bar', [])]), Expr([Atom('baz', [])])])])
    '''
    tokens = list(tokens)

    def parse_args(i):
        args = []
        expri = parse_expr(i)
        while expri:
            expr, i = expri
            args.append(expr)
            if i < len(tokens) and tokens[i].name == COMMA:
                i += 1
                expri = parse_expr(i)
            else:
                expri = None
        return args, i

    def parse_expr(i):
        loc = tokens[i].loc if i < len(tokens) else None
        parts = []
        parti = parse_part(i)
        while parti:
            part, i = parti
            parts.append(part)
            parti = parse_part(i)
        return Expr(parts, loc=loc), i

    def parse_part(i):
        if i >= len(tokens):
            return None
        elif tokens[i].name == NEWLINE:
            return NewLine(loc=tokens[i].loc), i+1
        elif tokens[i].name == COMMENT:
            return Comment(tokens[i].value, loc=tokens[i].loc), i+1
        elif tokens[i].name == WORD:
            return parse_atom(i)
        elif tokens[i].name == INT:
            return LitInt(tokens[i].value, loc=tokens[i].loc), i+1
        elif tokens[i].name == STR:
            return LitStr(tokens[i].value, loc=tokens[i].loc), i+1
        elif tokens[i].name == LPAREN:
            # LPAREN only allowed to modify word, not stand alone.
            raise SyntaxError("%s: Expected atom but got '('." % tokens[i].loc)

    def parse_atom(i):
        if i >= len(tokens):
            return None
        if tokens[i].name == WORD:
            word = tokens[i].value
            j = i+1
            while j < len(tokens) and tokens[j].name in {COMMENT, NEWLINE}:
                j += 1
            if j < len(tokens) and tokens[j].name == LPAREN:
                argsk = parse_args(j+1)
                if argsk:
                    args, k = argsk
                    if k < len(tokens) and tokens[k].name == RPAREN:
                        return Atom(word, args, loc=tokens[i].loc), k+1
                    elif k >= len(tokens):
                        raise SyntaxError("%s: Mismatched '('" % tokens[j].loc)
                    else:
                        raise SyntaxError("%s: Expected ')'" % tokens[k].loc)
            else:
                return Atom(word, [], loc=tokens[i].loc), i+1

    expr, i = parse_expr(0)
    if i >= len(tokens):
        return expr
    else:
        raise SyntaxError("%s: expected EOF" % tokens[i].loc)

class Term (Pure): pass

class Lit (Term):
    def __init__ (self, value, loc=None):
        self.value = value

    def expand(self, env):
        return self

    def compile(self, env):
        return lambda env: env.push(self.value)

    def decons(self, env):
        env.push((self.tag, self.value))

    def __repr__(self):
        return '%s(%r)' % (self.tag, self.value)

class LitInt  (Lit): tag = 'LitInt'
class LitStr  (Lit): tag = 'LitStr'
class LitExpr (Lit): tag = 'LitExpr'

class LitTuple (Term):
    tag = 'LitTuple'

    def __init__ (self, expr, loc=None):
        self.expr = expr
        self.loc = loc

    def expand(self, env):
        env2 = env.copy()
        return LitTuple(self.expr.expand(env2), self.loc)

    def compile(self, env):
        env2 = env.copy()
        exprfn = self.expr.compile(env2)
        def tuplefn(env3):
            env4 = env3.copy()
            env4.stack = []
            exprfn (env4)
            env3.push(tuple(env4.stack))
        return tuplefn

    def decons(self, env):
        env.push((self.tag, self.expr, self.loc))

    def __repr__(self):
        return 'LitTuple(%r)' % self.expr

class Atom (Term):
    tag = 'Atom'

    def __init__ (self, word, args, loc=None):
        self.word = word
        self.args = tuple(args)
        self.loc  = loc

    def expand(self, env):
        if self.word in env.macros:
            env.loc = self.loc
            return env.macros[self.word](env, *self.args).expand(env)
        else:
            expargs = []
            for arg in self.args:
                argenv = env.copy()
                expargs.append(arg.expand(argenv))
            return Atom(self.word, expargs, loc=self.loc)

    def compile(self, env):
        if self.word in env.words:
            wordfn = env.words[self.word]
            argfns = tuple(arg.compile(env) for arg in self.args)
            return (lambda env2: wordfn(env2, *argfns))
        elif self.loc:
            raise NameError("%s: Word not found: %s" % (self.loc, self.word))
        else:
            raise NameError("Word not found: %s" % self.word)

    def decons(self, env):
        env.push((self.tag, self.word, self.args, self.loc))

    def __repr__(self):
        return 'Atom(%r, %r)' % (self.word, list(self.args))

class Expr (Term):
    tag = 'Expr'

    def __init__ (self, parts, loc=None):
        self.parts = tuple(parts)
        self.loc = loc

    def expand(self, env):
        return Expr((part.expand(env) for part in self.parts), loc=self.loc)

    def compile(self, env):
        partfns = tuple(part.compile(env) for part in self.parts)
        def compfn(env2):
            for partfn in partfns:
                partfn(env2)
        return compfn

    def decons(self, env):
        env.push((self.tag, self.parts, self.loc))

    def __repr__(self):
        return 'Expr(%r)' % list(self.parts)

class Comment (Term):
    tag = 'Expr'

    def __init__ (self, comment, loc=None):
        self.comment = comment
        self.loc = loc

    def expand(self, env):
        return self

    def compile(self, env):
        return lambda env: None

    def decons(self, env):
        return env.push((self.tag, self.comment, self.loc))

    def __repr__(self):
        return 'Comment(%r)' % self.comment

class NewLine (Term):
    tag = 'NewLine'

    def __init__ (self, loc=None):
        self.loc = loc

    def expand(self, env):
        return self

    def compile(self, env):
        return lambda env: None

    def decons(self, env):
        return env.push((self.tag, self.loc))

    def __repr__(self):
        return 'NewLine()'

def can_drop(x):
    '''Is x safely droppable, i.e. can x be dropped without dropping
    some connection to the world, such as a file handle.

    >>> can_drop(1)
    True
    >>> can_drop([''])
    True
    >>> can_drop({'zero': 1})
    True
    >>> import sys
    >>> can_drop(sys.stdin)
    False
    >>> can_drop([sys.stdin])
    False
    '''

    if hasattr(x, 'can_drop'):
        return x.can_drop()
    if any(isinstance(x,t) for t in (list, tuple, set)):
        return all(can_drop(y) for y in x)
    if isinstance(x,dict):
        return (all(can_drop(y) for y in x.values())
            and all(can_drop(y) for y in x))
    return any(isinstance(x,ty) for ty in (int, bool, str, type(None)))

def can_dup(x):
    '''Is x safely duplicable, i.e. can x be duplicated without duplicating a
    mutable reference?

    >>> can_dup(10)
    True
    >>> can_dup("hello, world!")
    True
    >>> can_dup(True)
    True
    >>> can_dup(None)
    True
    >>> can_dup((1,2,3))
    True
    >>> can_dup((1,[2],3))
    False
    >>> can_dup([1,2,3])
    False
    >>> can_dup({1,2,3})
    False
    >>> can_dup({'a': 'b'})
    False
    '''
    if hasattr(x, 'can_dup'):
        return x.can_dup()
    if isinstance(x, tuple):
        return all(can_dup(y) for y in x)
    return any(isinstance(x,ty) for ty in (int, bool, str, type(None)))


# environment in which to execute and compile words

class Env:
    def __init__ (self, stack=(), words=None, macros=None, loc=None):
        self.stack = list(stack)
        self.words = words or copy.copy(builtin_words)
        self.macros = macros or copy.copy(builtin_macros)
        self.loc = loc

    def copy(self):
        return Env (
            stack  = (),
            words  = copy.copy(self.words),
            macros = copy.copy(self.macros),
        )

    def pop(self, word, n):
        if len(self.stack) < n:
            raise ValueError("Stack has fewer than %d arguments for %s" % (n, word))
        elif n == 1:
            return self.stack.pop()
        else:
            xs = self.stack[-n:]
            del self.stack[-n:]
            return xs

    def push(self, *args):
        self.stack.extend(args)

# built-in words
#
# these are given by python functions that take env as the first argument,
# then any word arguments (these are also python fns; higher order arguments
# are technically possible this way, but not in practice).

def make_word (name, n, m, fun):
    if m==1:
        if n==1:
            return lambda env: env.push(fun(env.pop(name,n)))
        else:
            return lambda env: env.push(fun(*env.pop(name,n)))
    else:
        if n==1:
            return lambda env: env.push(*fun(env.pop(name,n)))
        else:
            return lambda env: env.push(*fun(*env.pop(name,n)))

def dup(env):
    x = env.pop('dup', 1)
    if can_dup(x):
        env.push(x,x)
    else:
        raise ValueError("dup: Can't duplicate %r" % x)

def drop(env):
    x = env.pop('drop', 1)
    if not can_drop(x):
        raise ValueError("drop: Can't drop %r" % x)

def dip(env, fn):
    x = env.pop('dip', 1)
    fn(env)
    env.push(x)

def unpack2(env):
    t = env.pop('unpack2', 1)
    if isinstance(t, tuple) and len(t) == 2:
        x,y = t
        env.push(x,y)
    else:
        raise TypeError("unpack2: Expected pair, but got %r" % t)

def in_tuple(env, f):
    t = env.pop('in_tuple', 1)
    if isinstance(t, tuple):
        env2 = Env(stack=t)
        f(env2)
        env.push(tuple(env2.stack))

def decons (env):
    x = env.pop('decons',1)
    if hasattr(x, 'decons'):
        x.decons(env)
    else:
        raise TypeError("decons: Can't deconstruct %r" % x)

recons_types = {
    'Loc': Loc,
    'Token': Token,
    'LitInt': LitInt,
    'LitStr': LitStr,
    'LitExpr': LitExpr,
    'Atom': Atom,
    'Expr': Expr,
    'Comment': Comment,
    'NewLine': NewLine,
}

def recons (env):
    x = env.pop('recons',1)
    if not isinstance(x,tuple):
        raise TypeError("recons: Expected tuple.")
    if len(x) < 1:
        raise ValueError("recons: Missing tag.")
    if x[0] not in recons_types:
        raise ValueError("recons: Unknown tag %r" % x[0])
    cons = recons_types[x[0]]
    env.push(cons(*x[1:]))

builtin_words = {
    'in_tuple': in_tuple,
    'dip': dip,
    'dup': dup,
    'drop': drop,
    'swap': make_word('swap', 2, 2, lambda x,y: (y,x)),
    'pack2': make_word('pack2', 2, 1, lambda x,y: (x,y)),
    'unpack2': unpack2,
    '+' : make_word('+', 2, 1, lambda x,y: x + y),
    '-' : make_word('-', 2, 1, lambda x,y: x - y),
    '*' : make_word('*', 2, 1, lambda x,y: x * y),
    '/' : make_word('/', 2, 1, lambda x,y: x // y),
    '%' : make_word('%', 2, 1, lambda x,y: x % y),
    'decons': decons,
    'recons': recons,
    'let': make_word('len', 1, 1, len),
}


# built-in macros
#
# these are given by python functions that take env as the first argument,
# then all the arguments (un-expanded), and return the replacing code.

def let(env, *let_args):
    if len(let_args) == 2:
        if len(let_args[0].parts) != 1:
            raise ValueError("let: Expected LHS to have a single part.")
        if len(let_args[0].parts[0].args) != 0:
            raise ValueError("let: Higher order words not yet implemented.")
        name = let_args[0].parts[0].word
        env2 = env.copy()
        word = let_args[1].expand(env2).compile(env2)
        env.words[name] = word
        return Expr([])
    elif len(let_args) == 3:
        # ignore middle argument (it's a type signature)
        return let(env, let_args[0], let_args[2])
    else:
        raise ValueError("let: Expected exactly 2 or 3 arguments.")

def macro(env, *args):
    if len(args) == 2:
        if len(args[0].parts) != 1:
            raise ValueError("macro: Expected LHS to have a single part.")
        if len(args[0].parts[0].args) != 0:
            raise ValueError("macro: Higher order macros not allowed.")
        name = args[0].parts[0].word
        env2 = env.copy()
        word = args[1].expand(env2).compile(env2)
        def macrofn(env3, *varg):
            env3.push(env3, tuple(varg))
            word(env3)
            _,ast = env3.pop('macro', 2)
            return ast
        env.macros[name] = macrofn
        return Expr([])
    else:
        raise ValueError("macro: Expected exactly 2 arguments.")

def test(env, *test_args):
    if len(test_args) == 2:
        env2 = env.copy()
        env3 = env.copy()
        lhs = test_args[0].expand(env2).compile(env2)
        rhs = test_args[1].expand(env3).compile(env3)
        lhs(env2)
        rhs(env3)
        if env2.stack != env3.stack:
            loc_prefix = ('%s: ' % env.loc) if env.loc else ''
            print("%sTest failed %r vs %r\n" % (loc_prefix, env2.stack, env3.stack))
        return Expr([], loc=test_args[0].loc)
    else:
        raise ValueError("test: Expected exactly two arguments.")

def quote(f, expr):
    return LitExpr(expr)


builtin_macros = {
    'let': let,
    'macro': macro,
    'test': test,
    'quote': quote,
}


# running the thing!

def eval(code):
    '''
    >>> eval('')
    []
    >>> eval('10')
    [10]
    >>> eval('10 20')
    [10, 20]
    >>> eval('10 dup')
    [10, 10]
    >>> eval('10 20 30 dip(dup)')
    [10, 20, 20, 30]
    >>> eval('10 20 swap')
    [20, 10]
    >>> eval('"foo" "bar"')
    ['foo', 'bar']
    >>> eval('10 drop')
    []
    >>> eval('10 20 +')
    [30]
    >>> eval('30 20 10 dip(+)')
    [50, 10]
    >>> eval('let(trip, dup dup) 10 trip')
    [10, 10, 10]
    >>> eval('10 20 30 rot let(rot, swap dip(swap))')
    [30, 10, 20]
    '''
    env    = Env()
    tokens = tokenize(code)
    expr   = parse_tokens(tokens)
    expr2  = expr.expand(env)
    word   = expr2.compile(env)
    word(env)
    return env.stack

def run_file(filepath, env=None):
    env = env or Env()
    with open(filepath, 'r') as fp:
        tokens  = tokenize(fp.read(), loc=Loc(filepath))
        program = parse_tokens(tokens)
        program = program.expand(env)
        program = program.compile(env)
        program(env)

if __name__ == '__main__':
    import doctest
    doctest.testmod()

    import sys
    if len(sys.argv) > 1:
        env = Env()
        env.words['argv'] = tuple(sys.argv[1:])
        run_file(sys.argv[1])

