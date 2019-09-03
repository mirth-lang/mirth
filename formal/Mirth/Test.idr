{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

||| Minimalistic unit testing framework for idris.
module Mirth.Test

%access public export
%default total

data Test : Type where
  TestCase : (x: Type) -> {auto p: x} -> Test

example_tests : List Test
example_tests =
  [ TestCase (1 + 1 = 2)
  , TestCase (2 + 2 = 4)
  , TestCase ("foo" ++ "bar" = "foobar")
  ]

