module Main where

import qualified JSONSchema.Spec as JSONSchema
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "JSONSchema Tests"
    [ JSONSchema.tests
    ]
