module Main (main) where

import Test.Hspec
import StackSetSpec (stackSetSpec)

main :: IO ()
main = hspec $ do
    describe "StackSet" stackSetSpec
