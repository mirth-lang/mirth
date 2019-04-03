-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Mirth.Syntax
  ( Atom (..)
  , Expr (..)
  , Args (..)
  , Word (..)
  , Vocab (..)
  , Name (..)
  , Lit (..)
  ) where

import Mirth.Prelude
import Mirth.Syntax.Loc

data Vocab
  = Local
  | Global !Text
  deriving (Eq, Ord, Show)

newtype Name
  = Name Text
  deriving (Eq, Ord, Show)

data Lit 
  = LitInt !Integer
  | LitStr !Text
  deriving (Eq, Ord, Show)

data Word = Word
  { wordVocab :: !Vocab
  , wordLName :: !(L Name)
  , wordLArgs :: !(L Args)
  } deriving (Eq, Ord, Show)

wordName :: Word -> Name
wordName = unL . wordLName

wordArgs :: Word -> Args
wordArgs = unL . wordLArgs

data Atom
  = AWord !Word 
  | ALit !Lit
  | AComma 
  | ALine 
  | AComment !Text -- line at the end is implied
  deriving (Eq, Ord, Show)

isSpaceAtom :: Atom -> Bool
isSpaceAtom = \case
  ALine -> True
  AComment _ -> True
  _ -> False

isCommaAtom :: Atom -> Bool
isCommaAtom = \case
  AComma -> True
  _ -> False

newtype Args
  = Args { argsLAtoms :: [L Atom] }
  deriving (Eq, Ord, Show)

argsAtoms :: Args -> [Atom]
argsAtoms = map unL . argsLAtoms

newtype Expr
  = Expr { exprLAtoms :: [L Atom] }
  deriving (Eq, Ord, Show)

exprAtoms :: Expr -> [Atom]
exprAtoms = map unL . exprLAtoms

splitArgs :: Args -> [Expr]
splitArgs (Args atoms) =
  let atoms' = filter (not . isSpaceAtom . unL) atoms
      exprs xs =
        let (l,r) = break (isCommaAtom . unL) xs
        in if null r then [Expr l] else Expr l : exprs r
  in exprs atoms'


data Decl
  = DWordDecl !WordDecl
  deriving (Eq, Ord, Show)

data WordDecl = WordDecl
  { wordDeclDocs :: ![Text]
  , wordDeclName :: !Name 
  , wordDeclArgs :: ![Name]
  , wordDeclSigs :: ![WordSigDecl]
  , wordDeclLets :: ![WordLetDecl]
  } deriving (Eq, Ord, Show)

data WordSigDecl = WordSigDecl
  { wordSigDeclLName :: !(L Name)
  , wordSigDeclLArgs :: ![(L Name, L TypeExpr)]
  , wordSigDeclLType :: !(L TypeExpr)
  } deriving (Eq, Ord, Show)

data WordLetDecl = WordLetDecl
  { wordLetDeclLPat :: !(L Pattern)
  , wordLetDeclLName :: !(L Name)
  , wordLetDeclLCop :: !(L CoPattern)
  , wordLetDeclLVal :: !(L Expr)
  } deriving (Eq, Ord, Show)

newtype Pattern = Pattern
  { patternLAtoms :: [L Atom] 
  } deriving (Eq, Ord, Show)

newtype CoPattern = CoPattern
  { coPatternLWords :: [L Word]
  } deriving (Eq, Ord, Show)


data TypeExpr = TypeExpr
  { typeExprDom :: !StackType
  , typeExprCod :: !StackType
  } deriving (Eq, Ord, Show)

data StackType
  = StackNil
  | StackVar !(L Name)
  | StackCons StackType !(L Word)
  deriving (Eq, Ord, Show)

