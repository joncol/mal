import Test.Tasty
import Test.Tasty.HUnit

import Mal

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Read and print tests"
  [ atomTests
  , stringTests
  , quoteTests
  , quasiquoteTests
  , unquoteTests
  , spliceUnquoteTests
  ]

atomTests :: TestTree
atomTests = testGroup "Read and print tests"
  [ testCase "Number atom" $
      rep "1" @?= Right "1"

  , testCase "Symbol atom" $
      rep "abc" @?= Right "abc"

  , testCase "Whitespace" $
      rep "  123   " @?= Right "123"

  , testCase "List" $
      rep "(123 456 789)" @?= Right "(123 456 789)"

  , testCase "Vector" $
      rep "[123 456 789]" @?= Right "[123 456 789]"

  , testCase "Hash-map with keyword keys" $
      rep "{:a 123 :b 456 :c 789}" @?= Right "{:a 123 :b 456 :c 789}"

  , testCase "Hash-map with string keys" $
      rep "{\"a\" 123 \"b\" 456 \"c\" 789}" @?=
      Right "{\"a\" 123 \"b\" 456 \"c\" 789}"

  , testCase "String" $
      rep "\"abc def\"" @?= Right "\"abc def\""

  , testCase "Keyword" $
      rep ":abc" @?= Right ":abc"
  ]

stringTests :: TestTree
stringTests = testGroup "Read and print tests"
  [ testCase "Parens" $
      rep "\"abc (1 2 3)\"" @?= Right "\"abc (1 2 3)\""

  , testCase "Escaped backslash" $
      rep "\"abc\\\\123\"" @?= Right "\"abc\\\\123\""

  , testCase "Escaped double-quote" $
      rep "\"abc\\\"123\"" @?= Right "\"abc\\\"123\""
  ]

quoteTests :: TestTree
quoteTests = testGroup "Quote tests"
  [ testCase "Quoted number" $
      rep "'1" @?= Right "(quote 1)"
  ]

quasiquoteTests :: TestTree
quasiquoteTests = testGroup "Quasiquote tests"
  [ testCase "Quasiquoted number" $
      rep "`1" @?= Right "(quasiquote 1)"
  ]

unquoteTests :: TestTree
unquoteTests = testGroup "Unquote tests"
  [ testCase "Unquoted number" $
      rep "~1" @?= Right "(unquote 1)"
  ]

spliceUnquoteTests :: TestTree
spliceUnquoteTests = testGroup "Splice-unquote tests"
  [ testCase "Splice-unquoted list" $
      rep "~@(1 2 3)" @?= Right "(splice-unquote (1 2 3))"
  ]
