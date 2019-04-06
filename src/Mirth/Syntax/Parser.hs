-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirth.Syntax.Parser where

import Mirth.Prelude
import Mirth.Syntax
import Mirth.Syntax.Loc
import Control.Monad
import Data.Void
import Data.List.Extra
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | Get location from parser state.
getLoc :: Parser Loc
getLoc = do
  p <- getSourcePos
  pure Loc
    { locPath = sourceName p
    , locLine = unPos (sourceLine p)
    , locCol  = unPos (sourceColumn p)
    }
--
-- | Annotate with location.
parseL :: Parser t -> Parser (L t)
parseL p = L <$> getLoc <*> p

-- | Is this a valid name character?
isNameChar :: Char -> Bool
isNameChar = (`notElem` (" \t\r\n()[]{},\"" :: String))

-- | Take name-like characters.
takeNameP :: Maybe String -> Parser Text
takeNameP descM = takeWhile1P descM isNameChar

data Base = Dec | Hex | Oct | Bin

-- | Is this a valid digit for integer literal?
isDigit :: Base -> Char -> Bool
isDigit base = (`elem` baseStr base)
  where
    baseStr :: Base -> String
    baseStr Dec = "0123456789"
    baseStr Hex = "0123456789abcdefABCDEF"
    baseStr Oct = "01234567"
    baseStr Bin = "01"

-- | Value of base.
baseVal :: Base -> Integer
baseVal Dec = 10 
baseVal Hex = 16 
baseVal Oct = 8 
baseVal Bin = 2

-- | Count the number of succs needed to get from one enum val to the next.
-- (Can be negative.)
countSuccs :: (Enum t, Integral i) => t -> t -> i
countSuccs a b = fromIntegral (fromEnum b - fromEnum a)

-- | Get the value of a digit at a certain base.
digitVal :: Base -> Char -> Integer
digitVal Hex c | 'A' <= c && c <= 'F' = countSuccs 'A' c + 10
digitVal Hex c | 'a' <= c && c <= 'f' = countSuccs 'a' c + 10
digitVal _ c = countSuccs '0' c


-- | Parse an integer literal.
parseInt :: Parser Integer
parseInt = label "integer literal" $ do
    name <- unpack <$> takeNameP Nothing
    let (prefixFn, rest) =
            case name of
                '+' : r -> (id, r)
                '-' : r -> (negate, r)
                r -> (id, r)
        (base, digits) =
            case rest of
                '0' : 'x' : ds -> (Hex, ds)
                '0' : 'o' : ds -> (Oct, ds)
                '0' : 'b' : ds -> (Bin, ds)
                '0' : 'd' : ds -> (Dec, ds)
                ds -> (Dec, ds)

    unless (all (isDigit base) digits && notNull digits) $
        fail "Invalid integer literal"

    pure . prefixFn $
      foldl (\a b -> baseVal base * a + digitVal base b) 0 digits



-- | Parse a string literal.
parseStr :: Parser Text
parseStr = label "string literal" $ do
  void (char '"')
  xs <- many (escapeSeq <|> nonEscape)
  void (char '"')
  pure (T.concat xs)
    where
      nonEscape :: Parser Text
      nonEscape = takeWhile1P Nothing (`notElem` ("\n\t\r\\\"" :: String))

      escapeSeq :: Parser Text
      escapeSeq = do
        void (char '\\')
        choice
          [ char '\\' >> pure "\\"
          , char '\"' >> pure "\""
          , char '\'' >> pure "\'"
          , char 'n'  >> pure "\n"
          , char 'r'  >> pure "\r"
          , char 't'  >> pure "\t"
          , char '\r' >> optional (char '\n') >> pure ""
          , char '\n' >> pure ""
          ]

-- | Parse any literal.
parseLit :: Parser Lit
parseLit
  = LitInt <$> parseInt
  <|> LitStr <$> parseStr

-- | Parse an LF or CRLF.
parseLine :: Parser ()
parseLine = void newline <|> void crlf <|> void (char '\r')

-- | Parse a (single-line) comment. A comment must begin with the
-- # character, be followed by whitespace, and ends at the first
-- LF or CRLF or CR after the #. The returned text does not contain
-- the whitespace after the # nor does it ontain the LF/CRLF/CR.
parseComment :: Parser Text
parseComment = do
  void (char '#')
  choice 
    [ parseLine >> pure "" -- "edge case" of a '#' followed by a newline
    , do
        void (char ' ' <|> char '\t')
        t <- takeWhileP (Just "comment text") (`notElem` ("\n\r" :: String))
        void (optional (parseLine))
        pure t
    , takeWhile1P Nothing (`notElem` (" \t\n\r" :: String)) >> fail "not a comment"
    , pure ""
    ]

-- | Parse a single comma.
parseComma :: Parser () 
parseComma = void (char ',')

-- | Parse a name.
parseName :: Parser Name
parseName = Name <$> takeNameP (Just "name")

ignoreW :: Parser ()
ignoreW = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')

-- | Parse word args.
parseArgs :: Parser Args
parseArgs = ignoreW >> Args <$> choice
  [ do
      char '(' >> ignoreW
      xs <- many (const <$> parseL parseAtom <*> ignoreW)
      char ')' >> ignoreW
      pure xs
  , pure []
  ]

-- | Parse a word.
parseWord :: Parser Word
parseWord = Word <$> parseName <*> parseArgs

-- | Parse an atom.
parseAtom :: Parser Atom
parseAtom = choice
  [ const AComma <$> parseComma
  , const ALine <$> parseLine
  , try (AComment <$> parseComment)
  , try (ALit <$> parseLit)
  , AWord <$> parseWord
  ]

-- | Parse expression.
parseExpr :: Parser Expr
parseExpr = ignoreW >> Expr <$> many (const <$> parseL parseAtom <*> ignoreW)

-- | Parse whole file.
parseFile :: Parser Expr
parseFile = const <$> parseExpr <*> eof

