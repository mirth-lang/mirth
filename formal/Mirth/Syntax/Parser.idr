module Mirth.Syntax.Parser

import Mirth.Loc
import Mirth.Token
import Mirth.Test

%access public export
%default total

record ParseEnv where
  constructor MkParseEnv
  parseEnvToks : List Token
  parseEnvLoc  : Loc

record ParseError where
  constructor MkParseError
  parseErrorLoc : Loc
  parseErrorPos : Bool
  parseErrorMsg : String

record ParseResult (t: Type) where
  constructor MkParseResult
  parseResultVal : t
  parseResultPos : Bool
  parseResultEnv : ParseEnv

Functor ParseResult where
  map f (MkParseResult v b env) = MkParseResult (f v) b env

record Parser (t: Type) where
  constructor MkParser
  runParser : ParseEnv -> Either ParseError (ParseResult t)

Functor Parser where
  map f p = MkParser (\env => map (map f) (runParser p env))

Applicative Parser where
  pure x = MkParser (\env => pure (MkParseResult x False env))
  (<*>) pf px = MkParser $ \env1 =>
    case runParser pf env1 of
      Left err => Left err
      Right (MkParseResult f posf env2) =>
        case runParser px env2 of
          Left (MkParseError loc posx msg) =>
            Left (MkParseError loc (posf || posx) msg)
          Right (MkParseResult x posx env3) =>
            Right (MkParseResult (f x) (posf || posx) env3)

Monad Parser where
  (>>=) px py = MkParser $ \env1 =>
    case runParser px env1 of
      Left err => Left err
      Right (MkParseResult x posx env2) =>
        case runParser (py x) env2 of
          Left (MkParseError loc posy msg) =>
            Left (MkParseError loc (posx || posy) msg)
          Right (MkParseResult y posy env3) =>
            Right (MkParseResult y (posx || posy) env3)

fail : String -> Parser t
fail msg = MkParser $ \env =>
  Left (MkParseError (parseEnvLoc env) False msg)

peekWith : (List Token -> t) -> Parser t
peekWith f = MkParser $ \env =>
  pure (MkParseResult (f (parseEnvToks env)) False env)

peek : Parser (Maybe Token)
peek = peekWith head'

nextM : Parser (Maybe Token)
nextM = MkParser $ \env =>
  case parseEnvToks env of
    []         => pure (MkParseResult Nothing False env)
    [x]        => pure (MkParseResult (Just x) True (MkParseEnv [] (tokLoc x)))
    (x::y::ys) => pure (MkParseResult (Just x) True (MkParseEnv (y::ys) (tokLoc y)))

eof : Parser ()
eof = do
  b <- peekWith isNil
  if b
    then pure ()
    else fail ("Expected EOF.")

next : String -> Parser Token
next expected = do
  tokm <- nextM
  case tokm of
    Nothing => fail ("Expected " ++ expected ++ ".")
    Just tok => pure tok

nextIf : (Token -> Bool) -> String -> Parser Token
nextIf predicate expected = do
  tokm <- peek
  case tokm of
    Nothing => fail ("Expected " ++ expected ++ ".")
    Just tok =>
      if predicate tok then
        next expected
      else
        fail ("Expected " ++ expected ++ ".")

star : Parser t -> Parser (List t)
star p = MkParser $ aux [] False
  where
    aux : List t -> Bool -> ParseEnv -> Either ParseError (ParseResult (List t))
    aux acc b env =
      case runParser p env of
        Left (MkParseError _ False _) => Right (MkParseResult (reverse acc) b env)
        Left err => Left err
        Right (MkParseResult x False env') =>
          Right (MkParseResult (reverse (x::acc)) b env')
        Right (MkParseResult x True env') => do
          aux (x :: acc) True (assert_smaller env env')


parse : (path: Maybe String) -> (code: String) -> (parser: Parser t) -> Either ParseError t
parse path code parser =
  parseResultVal <$> runParser parser
    (MkParseEnv (tokens path code) (initialLoc path))

