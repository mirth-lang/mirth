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
    Rule(ATOMIC, COMMENT, r'#([\r\n]+|[ \t\n])'),
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
        self.path = assert_type_or_none(path, str, 'path')
        self.row = assert_type(row, int, 'row')
        self.col = assert_type(col, int, 'col')

    def __str__ (self):
        if self.path:
            return '%s:%d:%d' % (self.path, self.row, self.col)
        else:
            return '%d:%d' % (self.row, self.col)

    def __repr__ (self):
        return 'Loc(%r, %r, %r)' % (self.path, self.row, self.col)

    def next(self, text, **etc):
        TABSTOP = etc.get('TABSTOP', 8)
        row, col = self.row, self.col
        for c in text:
            if   c == '\n': row += 1 ; col = 1
            elif c == '\t': col += TABSTOP - col % TABSTOP
            else: col += 1
        return Loc(self.path, row, col)


class Token:
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

# rewriting this in OO style was a mistake ...
class Lexer:
    def __init__(self, source, loc=Loc(), rules=base_rules):
        self.source = assert_type(source, str, 'source')
        self.loc    = assert_type(loc, Loc, 'loc')
        self.rules  = rules

    def can_dup(self):
        return False # contains variables, so can't duplicate

    def can_drop(self):
        return True

    def __iter__(self):
        return self

    def __next__(self):
        r'''Get the next token based on rules.
        If no rule matches / end of file, returns None.
        If multiple rules match, returns the longest match.
        If multiple rules match with the same length, returns the first such match
        (the rules are ordered).

        >>> l = Lexer(''); exception(lambda: l.__next__())
        StopIteration()
        >>> l = Lexer('123'); l.__next__()
        Token('ATOMIC', 'INT', '123', 123, Loc(None, 1, 1))
        >>> l = Lexer('0xdeadbeef', Loc(None,100)); l.__next__()
        Token('ATOMIC', 'INT', '0xdeadbeef', 3735928559, Loc(None, 100, 1))
        >>> l = Lexer('1st', Loc(None,10,20)); l.__next__()
        Token('ATOMIC', 'WORD', '1st', '1st', Loc(None, 10, 20))
        >>> l = Lexer('"\l"', Loc('jeff.txt',2,3)); exception(lambda: l.__next__())
        SyntaxError('jeff.txt:2:3: Unknown escape sequence: \\l',)
        >>> l = Lexer('foo(bar baz)'); l.__next__()
        Token('ATOMIC', 'WORD', 'foo', 'foo', Loc(None, 1, 1))
        '''
        result = None
        best_len = 0
        for rule in self.rules:
            match = rule.regex.match(self.source)
            if match:
                text = match.group()
                if len(text) > best_len:
                    best_len = len(text)
                    try:
                        value = rule.to_value(text)
                    except SyntaxError as e:
                        raise SyntaxError("%s: %s" % (self.loc, e.msg))
                    result = Token(rule.kind, rule.name, text, value, self.loc)

        if result:
            self.source = self.source[best_len:]
            self.loc = result.next_loc()
            return result
        elif len(self.source) > 0:
            short_source = (self.source if len(self.source) < 8
                                      else self.source[:5] + '...')
            raise SyntaxError("%s: expected token, got %r"
                            % (show_loc(loc), short_source))
        else:
            raise StopIteration


def tokenize(source, loc=Loc(), rules=base_rules):
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
    return [t for t in Lexer(source, loc, rules) if t.kind != IGNORE]

def parse_tokens (tokens):
    r'''Parse tokens into a syntax tree.

    >>> parse_tokens(tokenize(''))
    Expr([])
    >>> parse_tokens(tokenize('10 20 30'))
    Expr([Lit(10), Lit(20), Lit(30)])
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
        parts = []
        parti = parse_part(i)
        while parti:
            part, i = parti
            parts.append(part)
            parti = parse_part(i)
        return Expr(parts), i

    def parse_part(i):
        if i >= len(tokens):
            return None
        elif tokens[i].name == NEWLINE:
            return tokens[i], i+1
        elif tokens[i].name == WORD:
            return parse_atom(i)
        elif tokens[i].name == INT:
            return Lit(tokens[i].value), i+1
        elif tokens[i].name == STR:
            return Lit(tokens[i].value), i+1
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
                        return Atom(word, args), k+1
                    elif k >= len(tokens):
                        raise SyntaxError("%s: Mismatched '('" % tokens[j].loc)
                    else:
                        raise SyntaxError("%s: Expected ')'" % tokens[k].loc)
            else:
                return Atom(word, []), i+1

    expr, i = parse_expr(0)
    if i >= len(tokens):
        return expr
    else:
        raise SyntaxError("%s: expected EOF" % tokens[i].loc)


class Lit:
    def __init__ (self, value):
        self.value = value

    def expand(self, env):
        return self

    def compile(self, env):
        return lambda env: env.push(self.value)

    def can_dup(self):
        return can_dup(self.value)

    def can_drop(self):
        return can_drop(self.value)

    def __repr__(self):
        return 'Lit(%r)' % self.value

class Atom:
    def __init__ (self, word, args):
        self.word = word
        self.args = tuple(args)

    def expand(self, env):
        if self.word in env.macros:
            return env.macros[self.word](env, *self.args).expand(env)
        else:
            expargs = []
            for arg in self.args:
                argenv = env.copy()
                expargs.append(arg.expand(argenv))
            return Atom(self.word, expargs)

    def compile(self, env):
        if self.word in env.words:
            wordfn = env.words[self.word]
            argfns = tuple(arg.compile(env) for arg in self.args)
            return (lambda env2: wordfn(env2, *argfns))
        else:
            raise NameError("Word not found: %s" % self.word)

    def can_dup(self):
        return all(can_dup(p) for p in self.args)

    def can_drop(self):
        return all(can_drop(p) for p in self.args)

    def __repr__(self):
        return 'Atom(%r, %r)' % (self.word, list(self.args))

class Expr:
    def __init__ (self, parts):
        self.parts = tuple(parts)

    def expand(self, env):
        return Expr(part.expand(env) for part in self.parts)

    def compile(self, env):
        partfns = tuple(part.compile(env) for part in self.parts)
        def compfn(env2):
            for partfn in partfns:
                partfn(env2)
        return compfn

    def can_dup(self):
        return all(can_dup(p) for p in self.parts)

    def can_drop(self):
        return all(can_drop(p) for p in self.parts)

    def __repr__(self):
        return 'Expr(%r)' % list(self.parts)

class Comment (Pure):
    def __init__ (self, comment):
        self.comment = comment

    def expand(self, env):
        return self

    def compile(self, env):
        return lambda env: None

class NewLine (Pure):
    def __init__ (self):
        pass

    def expand(self, env):
        return self

    def compile(self, env):
        return lambda env: None

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
    def __init__ (self, init_stack=(), words=None, macros=None):
        self.stack = list(init_stack)
        self.words = words or {
            'dup': dup,
            'drop': drop,
            'swap': swap,
            'dip': dip,
            'pack2': pack2,
            'unpack2': unpack2,
            'in_tuple': in_tuple,
            '+': plus,
        }
        self.macros = macros or {
            'let': let,
            #'let_macro': let_macro,
            'quote_expr': quote_expr,
        }

    def copy(self):
        return Env (
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

def dup(env):
    x = env.pop('dup', 1)
    if can_dup(x):
        env.push(x,x)
    else:
        raise ValueError("Can't duplicate %r" % x)

def drop(env):
    x = env.pop('drop', 1)
    if not can_drop(x):
        raise ValueError("Can't drop %r" % x)

def swap(env):
    a,b = env.pop('swap', 2)
    env.push(b,a)

def dip(env, fn):
    x = env.pop('dip', 1)
    fn(env)
    env.push(x)

def plus(env):
    x,y = env.pop('+', 2)
    env.push(x + y)

def pack2(env):
    x,y = env.pop('pack2', 2)
    env.push((x,y))

def unpack2(env):
    t = env.pop('unpack2', 1)
    if isinstance(t, tuple) and len(t) == 2:
        x,y = t
        env.push(x,y)
    else:
        raise TypeError("Expected pair, but got %r" % t)

def in_tuple(env, f):
    t = env.pop('in_tuple', 1)
    if isinstance(t, tuple):
        env2 = Env(init_stack=t)
        f(env2)
        env.push(tuple(env2.stack))

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
    else:
        raise ValueError("let: Expected exactly two arguments.")

def quote_expr(f,expr):
    return Lit(expr)

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

