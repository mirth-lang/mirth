-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirth.Syntax.Loc
  ( Loc (..)
  , L (..)
  , loc
  , unL
  , locStart
  , locSkip
  , locTag
  ) where

import Mirth.Prelude
import qualified Data.Text as T

data Loc = Loc
  { locPath :: !FilePath
  , locLine :: !Int
  , locCol  :: !Int
  } deriving (Eq, Ord, Show)

data L t = L !Loc !t
  deriving (Eq, Ord, Show)

loc :: L t -> Loc
loc (L a _) = a

unL :: L t -> t
unL (L _ b) = b

locStart :: FilePath -> Loc
locStart path = Loc path 1 1

locSkip :: Char -> Loc -> Loc
locSkip '\n' (Loc path l c) = Loc path (l+1) 0
locSkip _ (Loc path l c) = Loc path l (c+1)

locTag :: Loc -> Text
locTag (Loc path line col) = T.concat
  [ pack path
  , ":"
  , pack (show line)
  , ":"
  , pack (show col)
  ]

