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
      rep "1" @?= "1"

  , testCase "Symbol atom" $
      rep "abc" @?= "abc"

  , testCase "Whitespace" $
      rep "  123   " @?= "123"

  , testCase "List" $
      rep "(123 456 789)" @?= "(123 456 789)"

  , testCase "String" $
      rep "\"abc def\"" @?= "\"abc def\""
  ]

stringTests :: TestTree
stringTests = testGroup "Read and print tests"
  [ testCase "Parens" $
      rep "\"abc (1 2 3)\"" @?= "\"abc (1 2 3)\""

  , testCase "Escaped backslash" $
      rep "\"abc\\\\123\"" @?= "\"abc\\\\123\""

  , testCase "Escaped double-quote" $
      rep "\"abc\\\"123\"" @?= "\"abc\\\"123\""
  ]
