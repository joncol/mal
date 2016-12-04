import Test.Tasty
import Test.Tasty.HUnit

import Mal

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Read and print tests" [atomTests, stringTests]

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

  , testCase "String" $
      rep "\"abc def\"" @?= Right "\"abc def\""
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
