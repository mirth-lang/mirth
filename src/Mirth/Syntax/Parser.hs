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
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Pos

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

-- | Annotate with location.
parseL :: Parser t -> Parser (L t)
parseL p = L <$> getLoc <*> p

-- | Parse an integer literal.
parseInt :: Parser Integer
parseInt = read <$> label "integer literal" (many digitChar)

-- | Parse a string literal.
parseStr :: Parser Text
parseStr = label "string literal" $ do
  char '"'
  xs <- some (escapeSeq <|> nonEscape)
  char '"'
  pure (T.concat xs)
    where
      nonEscape :: Parser Text
      nonEscape = takeWhile1P Nothing (\c -> c /= '"' || c /= '\\' || c /= '\n' || c /= '\r')

      escapeSeq :: Parser Text
      escapeSeq = do
        char '\\' 
        choice
          [ char '\\' >> pure "\\"
          , char 'n'  >> pure "\n"
          , char 'r'  >> pure "\r"
          , char 't'  >> pure "\t"
          , char '\n' >> pure ""
          , char '"'  >> pure "\""
          , char '\'' >> pure "\'"
          ]

-- | Parse any literal.
parseLit :: Parser Lit
parseLit
  = LitInt <$> parseInt
  <|> LitStr <$> parseStr

-- | Parse an LF or CRLF.
parseLine :: Parser ()
parseLine = void newline <|> void crlf

-- | Parse a (single-line) comment. A comment must begin with the
-- # character, be followed by whitespace, and ends at the first
-- LF or CRLF after the #. The returned text does not contain the-- whitespace after the #' nor does it ontain the LF/CRLF.
parseComment :: Parser Text
parseComment = do
  void (char '#')
  choice 
    [ parseLine >> pure "" -- "edge case" of a '#' followed by a newline
    , do
        void (char ' ' <|> char '\t')
        t <- takeWhileP (Just "comment text") (not . (== '\n'))
        parseLine
        pure t
    ]

-- | Parse a single comma.
parseComma :: Parser () 
parseComma = void (char ',')

-- | Parse a name.
parseName :: Parser Name
parseName = Name <$> takeWhile1P (Just "name") (`elem` (" \t\r\n()[]{}," :: String))

ignoreW :: Parser ()
ignoreW = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')

-- | Parse word args.
parseArgs :: Parser Args
parseArgs = ignoreW >> Args <$> choice
  [ do
      char '(' >> ignoreW
      xs <- some (const <$> parseL parseAtom <*> ignoreW)
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
parseExpr = ignoreW >> Expr <$> some (const <$> parseL parseAtom <*> ignoreW)

-- | Parse whole file.
parseFile :: Parser Expr
parseFile = const <$> parseExpr <*> eof

