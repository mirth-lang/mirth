
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec
import Mirth.Prelude
import Mirth.Syntax
import Mirth.Syntax.Loc
import Mirth.Syntax.Parser


main = defaultMain $ testGroup "Mirth"
  [ parserTests
  ]



assertError msg (Left e) = pure ()
assertError msg (Right v) =
  fail (msg <> ": expected error, got " <> show v)

assertParse parser code result =
  assertEqual (show code) (Right result) (useParser parser code)

testPath = "<test>"
useParser p t = runParser p testPath t

parserTests = testGroup "Mirth.Syntax.Parser"
  [ testCase "parseInt" $ do
      assertParse parseInt "0" 0
      assertParse parseInt "42" 42
      assertParse parseInt "100" 100
      assertParse parseInt "+30" 30
      assertParse parseInt "+0" 0
      assertParse parseInt "-777" (-777)
      assertParse parseInt "0777" 777
      assertParse parseInt "0xFF" 0xFF
      assertParse parseInt "0xdeadbeef" 0xdeadbeef
      assertParse parseInt "0xcABCDEF0" 0xcABCDEF0
      assertParse parseInt "0x890BCabc" 0x890BCABC
      assertParse parseInt "0o123" 83
      assertParse parseInt "0o10" 8
      assertParse parseInt "0b1001" 9
      assertParse parseInt "0b10000000" 128
      assertParse parseInt "-0x100" (-256)
      assertParse parseInt "+0o44" 36
      assertError "empty string" (useParser parseInt "")
      assertError "trailing letters" (useParser parseInt "1st")
      assertError "trailing symbols" (useParser parseInt "0+")
      assertError "decimal point" (useParser parseInt "0.1")
      assertError "invalid digit" (useParser parseInt "10FFFF")
      assertError "invalid hex digit" (useParser parseInt "0xABCDEFG")
      assertError "invalid oct digit" (useParser parseInt "0o90")
      assertError "invalid bin digit" (useParser parseInt "0b01210")
  , testCase "parseStr" $ do
      assertParse parseStr "\"\"" ""
      assertParse parseStr "\"foo\"" "foo"
      assertParse parseStr "\" a b c \"" " a b c "
      assertParse parseStr "\"\\\"\"" "\""
      assertParse parseStr "\"\\\n\"" ""
      assertParse parseStr "\"\\n\"" "\n"
      assertParse parseStr "\"\\t\"" "\t"
      assertParse parseStr "\"\\r\"" "\r"
      assertParse parseStr "\"\\\\\"" "\\"
      assertParse parseStr "\"\\\r\n\"" ""
      assertParse parseStr "\"\\\r\""  ""
      assertParse parseStr "\"foo\"\"bar\"" "foo"
      assertParse parseStr "\"\\'\"" "\'"
      assertError "unescaped lf" (useParser parseStr "\"\n\"")
      assertError "unescaped cr" (useParser parseStr "\"\r\"") 
      assertError "unescaped crlf" (useParser parseStr "\"\r\n\"") 
      assertError "space before" (useParser parseStr " \"foo\"") 
      assertError "tab before" (useParser parseStr "\t\"foo\"")
      assertError "forgot starting quote" (useParser parseStr "foo\"")
      assertError "forgot ending  quote" (useParser parseStr "\"foo")

  , testCase "parseLit" $ do
      assertParse parseLit "10" (LitInt 10)
      assertParse parseLit "-20" (LitInt (-20))
      assertParse parseLit "\"foo bar\"" (LitStr "foo bar")

  , testCase "parseLine" $ do
      assertParse parseLine "\n" ()
      assertParse parseLine "\r" ()
      assertParse parseLine "\r\n" ()
      assertError " \\n" (useParser parseLine " \n")

  , testCase "parseComma" $ do
      assertParse parseComma "," ()

  , testCase "parseComment" $ do
      assertParse parseComment "#\n" ""
      assertParse parseComment "#\r" ""
      assertParse parseComment "#\r\n" ""
      assertParse parseComment "#" ""
      assertParse parseComment "# foo bar\n" "foo bar"
      assertParse parseComment "# foo bar\r" "foo bar"
      assertParse parseComment "# foo bar\r\n" "foo bar"
      assertParse parseComment "# foo bar" "foo bar"
      assertError "#foo bar\\n" (useParser parseComment "#foo bar\n")

  , testCase "parseName" $ do
      assertParse parseName "foo" (Name "foo")
      assertParse parseName "1st" (Name "1st")
      assertParse parseName "1+1" (Name "1+1")
      assertParse parseName "#foo" (Name "#foo")
      assertParse parseName "foo bar" (Name "foo")
      assertParse parseName "foo\nbar" (Name "foo")
      assertParse parseName "foo\rbar" (Name "foo")
      assertParse parseName "foo\tbar" (Name "foo")
      assertParse parseName "foo(bar" (Name "foo")
      assertParse parseName "foo,bar" (Name "foo")
      assertParse parseName "foo)bar" (Name "foo")
      assertError "empty string" (useParser parseName "")
      assertError "space" (useParser parseName " foo")
      assertError "open paren" (useParser parseName "(foo")

  , testCase "parseArgs" $ do
      assertParse parseArgs "" (Args [])
      assertParse parseArgs "()" (Args [])
      assertParse parseArgs "(,)" (Args [L (Loc testPath 1 2) AComma])
      assertParse parseArgs " ( , )" (Args [L (Loc testPath 1 4) AComma])
      assertParse parseArgs " \t(\t  , )" (Args [L (Loc testPath 1 19) AComma])

  , testCase "parseWord" $ do
      assertParse parseWord "foo" (Word (Name "foo") (Args []))
      assertParse parseWord "bar" (Word (Name "bar") (Args []))
      assertParse parseWord "foo()" (Word (Name "foo") (Args []))
      assertParse parseWord "foo(\n)" (Word (Name "foo") (Args [L (Loc testPath 1 5) ALine]))

  , testCase "parseAtom" $ do
      assertParse parseAtom "\nfoo" ALine
      assertParse parseAtom "\rfoo" ALine
      assertParse parseAtom "\r\nfoo" ALine
      assertParse parseAtom "# foo \n bar" (AComment "foo ")
      assertParse parseAtom ", bar" AComma
      assertParse parseAtom "foo bar" (AWord (Word (Name "foo") (Args [])))
      assertParse parseAtom "100 bar" (ALit (LitInt 100))
      assertParse parseAtom "\"foo bar\" baz" (ALit (LitStr "foo bar"))
      assertParse parseAtom "#foo bar" (AWord (Word (Name "#foo") (Args [])))
      assertError "leading space" (useParser parseAtom " foo")
  
  , testCase "parseExpr" $ do
      assertParse parseExpr "   " (Expr [])
      assertParse parseExpr "  100 200  "
        (Expr [ L (Loc testPath 1 3) (ALit (LitInt 100))
              , L (Loc testPath 1 7) (ALit (LitInt 200))
              ])
      assertParse parseExpr " foo\r\nbar"
        (Expr [ L (Loc testPath 1 2) (AWord (Word (Name "foo") (Args [])))
              , L (Loc testPath 1 5) ALine
              , L (Loc testPath 2 1) (AWord (Word (Name "bar") (Args [])))
              ])
      assertParse parseExpr "foo)bar"
        (Expr [ L (Loc testPath 1 1) (AWord (Word (Name "foo") (Args [])))
              ]) 

  , testCase "parseFile" $ do
      assertParse parseFile "foo\nbar"
        (Expr [ L (Loc testPath 1 1) (AWord (Word (Name "foo") (Args [])))
              , L (Loc testPath 1 4) ALine
              , L (Loc testPath 2 1) (AWord (Word (Name "bar") (Args [])))
              ])
      assertError "mismatched paren" (useParser parseFile "foo)bar")
  ]


