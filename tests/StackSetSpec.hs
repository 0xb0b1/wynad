{-# LANGUAGE ScopedTypeVariables #-}

module StackSetSpec (stackSetSpec) where

import Test.Hspec
import Test.QuickCheck

import Wynad.StackSet
import Wynad.Wayland.Types (RationalRect(..))

import qualified Data.Map as M

-- | Simple layout for testing
data TestLayout a = TestLayout deriving (Show, Read, Eq)

-- | Screen detail for testing
newtype TestSD = TestSD Int deriving (Show, Read, Eq)

-- | Create a simple test StackSet
testStackSet :: StackSet String TestLayout Int Integer TestSD
testStackSet = new TestLayout ["1", "2", "3"] [TestSD 0]

stackSetSpec :: Spec
stackSetSpec = do
    describe "new" $ do
        it "creates a StackSet with the correct number of workspaces" $ do
            let ss = testStackSet
            length (workspaces ss) `shouldBe` 3

        it "sets the first workspace as current" $ do
            let ss = testStackSet
            currentTag ss `shouldBe` "1"

        it "creates empty stacks" $ do
            let ss = testStackSet
            peek ss `shouldBe` Nothing

    describe "insertUp" $ do
        it "inserts a window and focuses it" $ do
            let ss = insertUp 1 testStackSet
            peek ss `shouldBe` Just 1

        it "doesn't insert duplicates" $ do
            let ss = insertUp 1 $ insertUp 1 testStackSet
            length (index ss) `shouldBe` 1

        it "inserts above the current focus" $ do
            let ss = insertUp 2 $ insertUp 1 testStackSet
            index ss `shouldBe` [2, 1]
            peek ss `shouldBe` Just 2

    describe "delete" $ do
        it "removes a window" $ do
            let ss = delete 1 $ insertUp 1 testStackSet
            peek ss `shouldBe` Nothing

        it "moves focus down when deleting focused" $ do
            let ss = delete 2 $ insertUp 2 $ insertUp 1 testStackSet
            peek ss `shouldBe` Just 1

    describe "view" $ do
        it "switches to a different workspace" $ do
            let ss = view "2" testStackSet
            currentTag ss `shouldBe` "2"

        it "is idempotent on current workspace" $ do
            let ss = view "1" testStackSet
            currentTag ss `shouldBe` "1"

    describe "focusUp/focusDown" $ do
        it "cycles through windows" $ do
            let ss = insertUp 3 $ insertUp 2 $ insertUp 1 testStackSet
            peek ss `shouldBe` Just 3
            peek (focusDown ss) `shouldBe` Just 2
            peek (focusDown $ focusDown ss) `shouldBe` Just 1
            peek (focusDown $ focusDown $ focusDown ss) `shouldBe` Just 3  -- wrap

        it "focusUp is inverse of focusDown" $ do
            let ss = insertUp 3 $ insertUp 2 $ insertUp 1 testStackSet
            peek (focusUp $ focusDown ss) `shouldBe` peek ss

    describe "swapUp/swapDown" $ do
        it "swaps positions without changing focus" $ do
            let ss = insertUp 3 $ insertUp 2 $ insertUp 1 testStackSet
            -- Windows are [3, 2, 1], focus on 3
            let ss' = swapDown ss
            -- After swap down, 3 moves down: [2, 3, 1]
            peek ss' `shouldBe` Just 3

    describe "float/sink" $ do
        it "float adds to floating map" $ do
            let ss = float 1 (RationalRect 0 0 0.5 0.5) $ insertUp 1 testStackSet
            M.member 1 (floating ss) `shouldBe` True

        it "sink removes from floating map" $ do
            let ss = sink 1 $ float 1 (RationalRect 0 0 0.5 0.5) $ insertUp 1 testStackSet
            M.member 1 (floating ss) `shouldBe` False

    describe "shift" $ do
        it "moves focused window to another workspace" $ do
            let ss = shift "2" $ insertUp 1 testStackSet
            peek ss `shouldBe` Nothing  -- moved away
            peek (view "2" ss) `shouldBe` Just 1

    describe "member" $ do
        it "returns True for managed windows" $ do
            let ss = insertUp 1 testStackSet
            member 1 ss `shouldBe` True

        it "returns False for unmanaged windows" $ do
            member 99 testStackSet `shouldBe` False

    describe "findTag" $ do
        it "finds the workspace containing a window" $ do
            let ss = insertUp 1 testStackSet
            findTag 1 ss `shouldBe` Just "1"

        it "returns Nothing for unknown windows" $ do
            findTag 99 testStackSet `shouldBe` Nothing
