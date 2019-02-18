module Mirth.Syntax

import Mirth.Loc
import Mirth.Token
import Mirth.Test
import Mirth.Syntax.Parser

%default total
%access public export

Name : Type
Name = String

data Lit : Type where
  LInt : Integer -> Lit
  LStr : String -> Lit

mutual
  data Atom : Type where
    ALit  : Loc -> Lit  -> Atom
    AWord : Loc -> Name -> Args -> Atom
    ALine : Loc -> Maybe String -> Atom

  Expr : Type
  Expr = List Atom

  Args : Type
  Args = List Expr

atomLoc : Atom -> Loc
atomLoc (ALit  loc _  ) = loc
atomLoc (AWord loc _ _) = loc
atomLoc (ALine loc _  ) = loc

-- Token predicates

isLineToken : Token -> Bool
isLineToken tok =
  case tokVal tok of
    LINE _ => True
    _      => False

isAtomToken : Token -> Bool
isAtomToken tok =
  case tokVal tok of
    INT  _ => True
    STR  _ => True
    LINE _ => True
    WORD _ => True
    _      => False

isCommaToken : Token -> Bool
isCommaToken tok =
  case tokVal tok of
    COMMA => True
    _     => False

isEndExprToken : Token -> Bool
isEndExprToken tok =
  case tokVal tok of
    COMMA  => True
    RPAREN => True
    _      => False

-- Parsers

lineParser : Parser Atom
lineParser = do
  tok <- nextIf isLineToken "newline or comment"
  case tokVal tok of
    LINE p => pure (ALine (tokLoc tok) p)
    _ => fail "Expected newline or comment."

linesParser : Parser (List Atom)
linesParser = star lineParser

parseComma : Parser ()
parseComma = do
  _ <- nextIf isCommaToken "comma"
  pure ()

argsParser : Token -> Parser Args
exprParser : Parser Expr
atomParser : Parser (List Atom) -- may involve lines/comments between WORD and LPAREN
atomParser = assert_total $ do
  tok <- nextIf isAtomToken "atom"
  case tokVal tok of
    INT  n => pure [ALit (tokLoc tok) (LInt n)]

    STR  n => pure [ALit (tokLoc tok) (LStr n)]
    LINE p => pure [ALine (tokLoc tok) p]
    WORD name => do
      lines <- linesParser
      args  <- argsParser tok
      pure (lines ++ [AWord (tokLoc tok) name args])
    _ => fail "Expected atom."

exprParser = assert_total (map concat (star atomParser))

argsParser _ = assert_total $ do
  headTokM <- peek
  case map tokVal headTokM of
    Just LPAREN => do
      lparenTok <- next "left paren"
      firstExpr <- exprParser
      restExprs <- star $ do
        parseComma
        exprParser
      rparenTok <- nextIf isEndExprToken "right paren or comma"
      case tokVal rparenTok of
        RPAREN => pure (firstExpr :: restExprs)
        _ => fail "Expected right paren."
    _ => pure []

fileParser : Parser Expr
fileParser = do
  expr <- exprParser
  eof
  pure expr

-- parsing tests
testParser : List Test
testParser =
    [ testCase "" $ Right []
    , testCase "10" $ Right [ALit (MkLoc Nothing 1 1) (LInt 10)]
    , testCase "foo" $ Right [AWord (MkLoc Nothing 1 1) "foo" []]
    , testCase "foo()" $ Right [AWord (MkLoc Nothing 1 1) "foo" [[]]]
    , testCase "foo(bar)" $ Right [AWord (MkLoc Nothing 1 1) "foo" [[
                   AWord (MkLoc Nothing 1 5) "bar" []]]]
    , testCase "foo(" $ Left
        (MkParseError (MkLoc Nothing 1 4) True "Expected right paren or comma.")
    , testCase "foo," $ Left
        (MkParseError (MkLoc Nothing 1 4) True "Expected EOF.")
    ]
  where
    TestPassed : String -> Either ParseError Expr -> Type
    TestPassed c r = (parse Nothing c fileParser = r)

    testCase : (code: String) -> (result: Either ParseError Expr) ->
               {auto p: TestPassed code result} ->
               Test
    testCase code result {p} =
      TestCase (TestPassed code result) {p}


