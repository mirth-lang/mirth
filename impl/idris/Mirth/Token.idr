||| Tokens & Lexer for Mirth.
module Mirth.Token
import Mirth.Test
import Mirth.Loc

%access public export
%default total

data TokenVal : Type where
  LPAREN  : TokenVal
  RPAREN  : TokenVal
  COMMA   : TokenVal
  LINE    : Maybe String -> TokenVal
  INT     : Integer -> TokenVal
  STR     : String -> TokenVal
  WORD    : String -> TokenVal
  ERROR   : String -> TokenVal

record Token where
  constructor MkToken
  tokLoc : Loc
  tokStr : String
  tokVal : TokenVal


specialChars : List Char
specialChars = unpack " \t\n\r,()[]{}\""

isIgnoreChar : Char -> Bool
isIgnoreChar ' '  = True
isIgnoreChar '\t' = True
isIgnoreChar '\r' = True
isIgnoreChar _    = False

stripIgnore : Loc -> List Char -> (Loc, List Char)
stripIgnore loc [] = (loc, [])
stripIgnore loc (c::cs) =
  if isIgnoreChar c
     then stripIgnore (advanceChar c loc) cs
     else (loc, c::cs)

isWordChar : Char -> Bool
isWordChar c = (c > ' ') && (not (elem c specialChars))

inRange : Char -> Char -> Char -> Bool
inRange a b c = a <= b && b <= c

digitValue : Char -> Integer
digitValue c =
  case c of
    '0' => 0
    '1' => 1
    '2' => 2
    '3' => 3
    '4' => 4
    '5' => 5
    '6' => 6
    '7' => 7
    '8' => 8
    '9' => 9
    'A' => 10
    'B' => 11
    'C' => 12
    'D' => 13
    'E' => 14
    'F' => 15
    'a' => 10
    'b' => 11
    'c' => 12
    'd' => 13
    'e' => 14
    'f' => 15
    _ => 0

lexInt : List Char -> (base: Integer) -> (accum: Integer) -> Integer
lexInt [] base t = t
lexInt (c::cs) base t = lexInt cs base (base*t + digitValue c)

lexWord : List Char -> TokenVal
lexWord ('0'::'x'::cs) =
  if all isHexDigit cs && length cs > 0
     then INT (lexInt cs 16 0)
     else ERROR ("Invalid hex literal: 0x" ++ pack cs)
lexWord ('-'::cs) =
  if all isDigit cs && length cs > 0
     then INT (negate (lexInt cs 10 0))
     else WORD (pack ('-'::cs))
lexWord cs =
  if all isDigit cs
     then INT (lexInt cs 10 0)
     else WORD (pack cs)

nextToken : Loc -> List Char -> (Token, List Char)
nextToken loc ('('::cs) = (MkToken loc "(" LPAREN, cs)
nextToken loc (')'::cs) = (MkToken loc ")" RPAREN, cs)
nextToken loc (','::cs) = (MkToken loc "," COMMA, cs)
nextToken loc ('\n'::cs) = (MkToken loc "\n" (LINE Nothing), cs)
nextToken loc ('#'::cs) = aux ['#'] cs
  where
    aux : (acc: List Char) -> (code: List Char) -> (Token, List Char)
    aux acc ('\n'::cs) =
      let text = pack (reverse ('\n' :: acc))
          tok  = MkToken loc text (LINE (Just text))
      in (tok, cs)
    aux acc [] =
      let text = pack (reverse acc)
          tok  = MkToken loc text (LINE (Just text))
      in (tok, [])
    aux acc (c::cs) = aux (c::acc) cs

-- TODO Str literals.
nextToken loc cs with (span isWordChar cs)
  | ([], _  ) = (MkToken loc "" (ERROR "Expected token, got unknown symbol."), [])
  | (ws, cs') = let w = pack ws in (MkToken loc w (lexWord ws), cs')

tokens : (path: Maybe String) -> (code: String) -> List Token
tokens path code = aux (initialLoc path) (unpack code)
  where
    aux : (loc: Loc) -> (code: List Char) -> List Token
    aux loc cs with (stripIgnore loc cs)
      | (loc', []) = []
      | (loc', cs') =
        let (tok, cs'') = (nextToken loc' cs')
        in tok :: aux (advanceStr (tokStr tok) loc')
                      (assert_smaller cs cs'')


----------------
-- UNIT TESTS --
----------------

tokensTests : List Test
tokensTests =
    [ testCase "" []
    , testCase "foo" [(1,1,WORD "foo")]
    , testCase "foo bar" [(1,1,WORD "foo"), (1,5,WORD "bar")]
    , testCase "123" [(1,1,INT 123)]
    , testCase "1234567890" [(1,1,INT 1234567890)]
    , testCase "-10" [(1,1,INT (-10))]
    , testCase "0xABCDEF" [(1,1,INT 0xABCDEF)]
    , testCase "0xG" [(1,1,ERROR "Invalid hex literal: 0xG")]
    , testCase "0x"  [(1,1,ERROR "Invalid hex literal: 0x")]
    , testCase "0x FF" [(1,1,ERROR "Invalid hex literal: 0x"), (1,4,WORD "FF")]
    , testCase "1st" [(1,1,WORD "1st")]
    , testCase "test()" [(1,1,WORD "test"), (1,5,LPAREN), (1,6,RPAREN)]
    , testCase "foo,bar" [(1,1,WORD "foo"), (1,4,COMMA), (1,5,WORD "bar")]
    ]
  where
    TokList : Type
    TokList = List (Integer, Integer, TokenVal)

    tokList : List Token -> TokList
    tokList = map (\tok => (locRow (tokLoc tok), locCol (tokLoc tok), tokVal tok))

    Matches : String -> TokList -> Type
    Matches code expected = (expected = tokList (tokens Nothing code))

    testCase : (code: String) ->
               (toks: TokList) ->
               {auto p: Matches code toks} ->
               Test
    testCase code toks {p} = TestCase (Matches code toks) {p}


