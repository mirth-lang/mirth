-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE NoImplicitPrelude #-}

module Mirth.Prelude
  ( module X
  , Text, pack, unpack
  ) where

import Prelude as X hiding (Word)
import Data.Text (Text, pack, unpack)

