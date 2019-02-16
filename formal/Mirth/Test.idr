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

