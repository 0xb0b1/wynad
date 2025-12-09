{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.StackSet
-- Copyright   :  (c) Don Stewart 2007, (c) 2025 Paulo Vicente (Wynad adaptation)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- The 'StackSet' data type encodes a window manager abstraction.
-- This is adapted from XMonad's StackSet with minimal changes for Wynad.
--
-- The window manager is a set of virtual workspaces. On each workspace is a
-- stack of windows. A given workspace is always current, and a given
-- window on each workspace has focus. The focused window on the current
-- workspace is the one which will take user input.
--
-- We use a zipper to track focus, allowing us to have correct focus
-- by construction.
--
-----------------------------------------------------------------------------

module Wynad.StackSet
    ( -- * Types
      StackSet(..)
    , Workspace(..)
    , Screen(..)
    , Stack(..)

      -- * Construction
    , new

      -- * View operations
    , view
    , greedyView

      -- * Screen operations
    , lookupWorkspace
    , screens
    , workspaces
    , allWindows
    , currentTag

      -- * Stack operations
    , peek
    , index
    , integrate
    , integrate'
    , differentiate
    , focusUp
    , focusDown
    , focusUp'
    , focusDown'
    , focusMaster
    , focusWindow

      -- * Query operations
    , tagMember
    , renameTag
    , ensureTags
    , member
    , findTag
    , mapWorkspace
    , mapLayout

      -- * Modify operations
    , insertUp
    , delete
    , delete'
    , filter

      -- * Master operations
    , swapUp
    , swapDown
    , swapMaster
    , shiftMaster

      -- * Stack modification
    , modify
    , modify'
    , float
    , sink

      -- * Composite operations
    , shift
    , shiftWin

      -- * Testing
    , abort
    ) where

import Prelude hiding (filter)
import Control.Applicative.Backwards (Backwards (Backwards, forwards))
import Data.Foldable (foldr, toList)
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import qualified Data.List as L (deleteBy, find, splitAt, filter, nub)
import Data.List ((\\))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as M (Map, insert, delete, empty)

import Wynad.Wayland.Types (RationalRect(..))

-----------------------------------------------------------------------------
-- Core Types
-----------------------------------------------------------------------------

-- | A cursor into a non-empty list of workspaces.
--
-- We puncture the workspace list, producing a hole in the structure
-- used to track the currently focused workspace. The two other lists
-- that are produced are used to track those workspaces visible as
-- screens, and those workspaces not visible anywhere.
data StackSet i l a sid sd = StackSet
    { current  :: !(Screen i l a sid sd)  -- ^ Currently focused workspace
    , visible  :: [Screen i l a sid sd]   -- ^ Non-focused but visible workspaces
    , hidden   :: [Workspace i l a]       -- ^ Workspaces not visible anywhere
    , floating :: M.Map a RationalRect    -- ^ Floating windows
    }
    deriving (Show, Read, Eq)

-- | Visible workspaces and their screens.
data Screen i l a sid sd = Screen
    { workspace    :: !(Workspace i l a)
    , screen       :: !sid
    , screenDetail :: !sd
    }
    deriving (Show, Read, Eq)

-- | A workspace is just a tag, a layout, and a stack.
data Workspace i l a = Workspace
    { tag    :: !i
    , layout :: l
    , stack  :: Maybe (Stack a)
    }
    deriving (Show, Read, Eq)

-- | A stack is a cursor onto a window list.
-- The data structure tracks focus by construction, and
-- the master window is by convention the top-most item.
-- Focus operations will not reorder the list that results from
-- flattening the cursor.
data Stack a = Stack
    { focus :: !a   -- ^ Focused element
    , up    :: [a]  -- ^ Elements above focus (reversed)
    , down  :: [a]  -- ^ Elements below focus
    }
    deriving (Show, Read, Eq, Functor)

instance Foldable Stack where
    toList = integrate
    foldr f z = foldr f z . toList

instance Traversable Stack where
    traverse f s =
        flip Stack
            <$> forwards (traverse (Backwards . f) (up s))
            <*> f (focus s)
            <*> traverse f (down s)

-- | Indicate an error condition.
abort :: String -> a
abort x = error $ "wynad: StackSet: " ++ x

-----------------------------------------------------------------------------
-- Construction
-----------------------------------------------------------------------------

-- | /O(n)/. Create a new stackset, of empty stacks, with given tags,
-- with physical screens whose descriptions are given by 'm'. The
-- number of physical screens should be less than or equal to the
-- number of workspace tags. The first workspace will be current.
new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new l (wid:wids) (m:ms) | length ms <= length wids
    = StackSet cur visi (map ws unseen) M.empty
    where
        ws i = Workspace i l Nothing
        (seen, unseen) = L.splitAt (length ms) wids
        cur :| visi = Screen (ws wid) 0 m :| [ Screen (ws i) s sd | (i, s, sd) <- zip3 seen [1..] ms ]
new _ _ _ = abort "non-positive argument to StackSet.new"

-----------------------------------------------------------------------------
-- View Operations
-----------------------------------------------------------------------------

-- | /O(w)/. Set focus to the workspace with index 'i'.
-- If the index is out of range, return the original StackSet.
--
-- If the workspace is not visible on any screen, it becomes the current
-- screen. If it is in the visible list, it becomes current.
view :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view i s
    | i == currentTag s = s  -- already current

    | Just x <- L.find ((i ==) . tag . workspace) (visible s)
    -- if it is visible, it is just raised
    = s { current = x, visible = current s : L.deleteBy (equating screen) x (visible s) }

    | Just x <- L.find ((i ==) . tag) (hidden s)  -- must be hidden then
    -- if it was hidden, it is raised on the screen currently used
    = s { current = (current s) { workspace = x }
        , hidden = workspace (current s) : L.deleteBy (equating tag) x (hidden s) }

    | otherwise = s  -- not a member of the stackset
    where
        equating f x y = f x == f y

-- | Set focus to the given workspace. If that workspace does not exist,
-- the original workspace is returned. If that workspace is 'hidden', then
-- display that workspace on the current screen, and move the current
-- workspace to 'hidden'. If that workspace is 'visible' on another screen,
-- the workspaces of the current screen and the other screen are swapped.
greedyView :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView w ws
    | any wTag (hidden ws) = view w ws
    | Just s <- L.find (wTag . workspace) (visible ws)
        = ws { current = (current ws) { workspace = workspace s }
             , visible = s { workspace = workspace (current ws) }
                       : L.filter (not . wTag . workspace) (visible ws) }
    | otherwise = ws
    where wTag = (w ==) . tag

-----------------------------------------------------------------------------
-- Screen Operations
-----------------------------------------------------------------------------

-- | Find the tag of the workspace visible on screen 'sc'.
-- 'Nothing' if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]

-- | Get a list of all screens in the StackSet.
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s

-- | Get a list of all workspaces in the StackSet.
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

-- | Get a list of all windows in the StackSet in no particular order.
allWindows :: Eq a => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

-- | Get the tag of the currently focused workspace.
currentTag :: StackSet i l a s sd -> i
currentTag = tag . workspace . current

-----------------------------------------------------------------------------
-- Stack Operations
-----------------------------------------------------------------------------

-- | The 'with' function takes a default value, a function, and a
-- StackSet. If the current stack is Nothing, 'with' returns the
-- default value. Otherwise, it applies the function to the stack.
with :: b -> (Stack a -> b) -> StackSet i l a s sd -> b
with dflt f = maybe dflt f . stack . workspace . current

-- | Apply a function, and a default value for 'Nothing', to modify the current stack.
modify :: Maybe (Stack a) -> (Stack a -> Maybe (Stack a)) -> StackSet i l a s sd -> StackSet i l a s sd
modify d f s = s { current = (current s)
                        { workspace = (workspace (current s)) { stack = with d f s }}}

-- | Apply a function to modify the current stack if it isn't empty.
modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = modify Nothing (Just . f)

-- | /O(1)/. Extract the focused element of the current stack.
-- Return 'Just' that element, or 'Nothing' for an empty stack.
peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (return . focus)

-- | /O(n)/. Flatten a Stack into a list.
integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

-- | /O(n)/. Flatten a possibly empty stack into a list.
integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate

-- | /O(n)/. Turn a list into a possibly empty stack (i.e., a zipper):
-- the first element of the list is current, and the rest is down.
differentiate :: [a] -> Maybe (Stack a)
differentiate []     = Nothing
differentiate (x:xs) = Just $ Stack x [] xs

-- | /O(n)/. Filter elements from a stack.
-- Order is preserved, and focus moves as described for 'delete'.
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs) = case L.filter p (f:rs) of
    f':rs' -> Just $ Stack f' (L.filter p ls) rs'
    []     -> case L.filter p ls of
                f':ls' -> Just $ Stack f' ls' []
                []     -> Nothing

-- | /O(s)/. Extract the stack on the current workspace, as a list.
index :: StackSet i l a s sd -> [a]
index = with [] integrate

-- | /O(1), O(w) on wrapping/. Move the window focus up the stack, wrapping.
focusUp :: StackSet i l a s sd -> StackSet i l a s sd
focusUp = modify' focusUp'

-- | /O(1), O(w) on wrapping/. Move the window focus down the stack.
focusDown :: StackSet i l a s sd -> StackSet i l a s sd
focusDown = modify' focusDown'

-- | Variant of focusUp that works on a Stack.
focusUp' :: Stack a -> Stack a
focusUp' (Stack t (l:ls) rs) = Stack l ls (t:rs)
focusUp' (Stack t []     rs) = Stack x xs []
    where (x :| xs) = NE.reverse (t :| rs)

-- | Variant of focusDown that works on a Stack.
focusDown' :: Stack a -> Stack a
focusDown' = reverseStack . focusUp' . reverseStack

-- | Reverse a stack.
reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls

-- | /O(1), O(w) on wrapping/. Swap the upwards neighbour in the stack.
swapUp :: StackSet i l a s sd -> StackSet i l a s sd
swapUp = modify' swapUp'

-- | /O(1), O(w) on wrapping/. Swap the downwards neighbour.
swapDown :: StackSet i l a s sd -> StackSet i l a s sd
swapDown = modify' (reverseStack . swapUp' . reverseStack)

-- | Variant of swapUp that works on a Stack.
swapUp' :: Stack a -> Stack a
swapUp' (Stack t (l:ls) rs) = Stack t ls (l:rs)
swapUp' (Stack t []     rs) = Stack t (reverse rs) []

-- | /O(1) on current window, O(n) in general/. Focus the window 'w',
-- and set its workspace as current.
focusWindow :: (Eq s, Eq a, Eq i) => a -> StackSet i l a s sd -> StackSet i l a s sd
focusWindow w s | Just w == peek s = s
                | otherwise        = fromMaybe s $ do
                    n <- findTag w s
                    return $ until ((Just w ==) . peek) focusUp (view n s)

-- | /O(s)/. Set focus to the master window.
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' $ \c -> case c of
    Stack _ []     _  -> c
    Stack t (l:ls) rs -> Stack x [] (xs ++ t : rs) where (x :| xs) = NE.reverse (l :| ls)

-----------------------------------------------------------------------------
-- Query Operations
-----------------------------------------------------------------------------

-- | Is the given tag present in the StackSet?
tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . map tag . workspaces

-- | Rename a given tag if present in the StackSet.
renameTag :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
renameTag o n = mapWorkspace rename
    where rename w = if tag w == o then w { tag = n } else w

-- | Ensure that a given set of workspace tags is present.
ensureTags :: Eq i => l -> [i] -> StackSet i l a s sd -> StackSet i l a s sd
ensureTags l allt st = et allt (map tag (workspaces st) \\ allt) st
    where
        et [] _ s = s
        et (i:is) rn s | i `tagMember` s = et is rn s
        et (i:is) [] s = et is [] (s { hidden = Workspace i l Nothing : hidden s })
        et (i:is) (r:rs) s = et is rs $ renameTag r i s

-- | Map a function on all the workspaces in the StackSet.
mapWorkspace :: (Workspace i l a -> Workspace i l a) -> StackSet i l a s sd -> StackSet i l a s sd
mapWorkspace f s = s { current = updScr (current s)
                     , visible = map updScr (visible s)
                     , hidden  = map f (hidden s) }
    where updScr scr = scr { workspace = f (workspace scr) }

-- | Map a function on all the layouts in the StackSet.
mapLayout :: (l -> l') -> StackSet i l a s sd -> StackSet i l' a s sd
mapLayout f (StackSet v vs hs m) = StackSet (fScreen v) (map fScreen vs) (map fWorkspace hs) m
    where
        fScreen (Screen ws s sd) = Screen (fWorkspace ws) s sd
        fWorkspace (Workspace t l s) = Workspace t (f l) s

-- | /O(n)/. Is a window in the StackSet?
member :: Eq a => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)

-- | /O(1) on current window, O(n) in general/.
-- Return 'Just' the workspace tag of the given window, or 'Nothing'
-- if the window is not in the StackSet.
findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag a s = listToMaybe
    [ tag w | w <- workspaces s, has a (stack w) ]
    where
        has _ Nothing              = False
        has x (Just (Stack t l r)) = x `elem` (t : l ++ r)

-----------------------------------------------------------------------------
-- Modify Operations
-----------------------------------------------------------------------------

-- | /O(n)/. Insert a new element into the stack, above the currently
-- focused element. The new element is given focus.
--
-- If the element is already in the stackset, the original stackset is
-- returned unmodified.
insertUp :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp a s = if member a s then s else insert
    where insert = modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack a l (t:r)) s

-- | /O(1) on current window, O(n) in general/. Delete window 'w' if it exists.
delete :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete w = sink w . delete' w

-- | Only temporarily remove the window from the stack.
delete' :: (Eq a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete' w s = s { current = removeFromScreen        (current s)
                , visible = map removeFromScreen    (visible s)
                , hidden  = map removeFromWorkspace (hidden  s) }
    where
        removeFromWorkspace ws = ws { stack = stack ws >>= filter (/= w) }
        removeFromScreen scr   = scr { workspace = removeFromWorkspace (workspace scr) }

-----------------------------------------------------------------------------
-- Floating Operations
-----------------------------------------------------------------------------

-- | Given a window and its preferred rectangle, set it as floating.
float :: Ord a => a -> RationalRect -> StackSet i l a s sd -> StackSet i l a s sd
float w r s = s { floating = M.insert w r (floating s) }

-- | Clear the floating status of a window.
sink :: Ord a => a -> StackSet i l a s sd -> StackSet i l a s sd
sink w s = s { floating = M.delete w (floating s) }

-----------------------------------------------------------------------------
-- Master Operations
-----------------------------------------------------------------------------

-- | /O(s)/. Set the master window to the focused window.
-- The old master window is swapped with the focused window.
swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster = modify' $ \c -> case c of
    Stack _ []     _  -> c
    Stack t (l:ls) rs -> Stack t [] (xs ++ x : rs) where (x :| xs) = NE.reverse (l :| ls)

-- | /O(s)/. Set the master window to the focused window.
-- Other windows are shifted down on the stack.
shiftMaster :: StackSet i l a s sd -> StackSet i l a s sd
shiftMaster = modify' $ \c -> case c of
    Stack _ [] _ -> c
    Stack t ls rs -> Stack t [] (reverse ls ++ rs)

-----------------------------------------------------------------------------
-- Composite Operations
-----------------------------------------------------------------------------

-- | /O(w)/. Move the focused element to stack 'n', leaving it focused there.
shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

-- | /O(n)/. Move window 'w' to stack 'n', leaving it focused there.
shiftWin :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin n w s = case findTag w s of
                    Just from | n `tagMember` s && n /= from -> go from s
                    _                                        -> s
    where go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)

-- | Apply a function on a specific workspace.
onWorkspace :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
            -> (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s
