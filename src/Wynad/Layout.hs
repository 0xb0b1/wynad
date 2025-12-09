{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Layout
-- Copyright   :  (c) Spencer Janssen 2007, (c) 2025 Paulo Vicente (Wynad adaptation)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- The collection of core layouts for Wynad.
-- Adapted from XMonad's layout system.
--
-----------------------------------------------------------------------------

module Wynad.Layout
    ( -- * Layout Class
      LayoutClass(..)
    , Layout(..)
    , readsLayout

      -- * Core Layouts
    , Full(..)
    , Tall(..)
    , Mirror(..)

      -- * Layout Messages
    , Resize(..)
    , IncMasterN(..)
    , ChangeLayout(..)
    , JumpToLayout(..)
    , LayoutMessages(..)

      -- * Layout Combinators
    , Choose(..)
    , (|||)
    , CLR(..)

      -- * Message Type
    , Message
    , SomeMessage(..)
    , fromMessage

      -- * Utility Functions
    , mirrorRect
    , splitVertically
    , splitHorizontally
    , splitVerticallyBy
    , splitHorizontallyBy
    , tile
    ) where

import Wynad.Wayland.Types (Rectangle(..))
import qualified Wynad.StackSet as W

import Control.Arrow ((***), second)
import Control.Monad (msum, liftM2, join)
import Data.Maybe (fromMaybe)
import Data.Typeable

-----------------------------------------------------------------------------
-- Message System
-----------------------------------------------------------------------------

-- | User-extensible messages must be a member of this class.
class Typeable a => Message a

-- | A wrapped value of some type in the 'Message' class.
data SomeMessage = forall a. Message a => SomeMessage a

-- | Unwrap a message, performing a dynamic type check.
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-----------------------------------------------------------------------------
-- Layout Messages
-----------------------------------------------------------------------------

-- | Messages that all layouts should consider handling.
data LayoutMessages
    = Hide              -- ^ Sent when a layout becomes non-visible
    | ReleaseResources  -- ^ Sent when wynad is exiting or restarting
    deriving (Eq, Show)

instance Message LayoutMessages

-- | Change the size of the master pane.
data Resize = Shrink | Expand
    deriving (Eq, Show)

instance Message Resize

-- | Increase the number of clients in the master pane.
newtype IncMasterN = IncMasterN Int
    deriving (Eq, Show)

instance Message IncMasterN

-- | Messages to change the current layout.
data ChangeLayout = FirstLayout | NextLayout
    deriving (Eq, Show)

instance Message ChangeLayout

-- | A message to jump to a particular layout by description.
newtype JumpToLayout = JumpToLayout String
    deriving (Eq, Show)

instance Message JumpToLayout

-- | Internal message for layout switching.
data NextNoWrap = NextNoWrap
    deriving (Eq, Show)

instance Message NextNoWrap

-----------------------------------------------------------------------------
-- Layout Class
-----------------------------------------------------------------------------

-- | Every layout must be an instance of 'LayoutClass'.
--
-- Minimal complete definition: either 'runLayout' or both 'doLayout' and 'emptyLayout'.
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | By default, 'runLayout' calls 'doLayout' if there are any
    -- windows to be laid out, and 'emptyLayout' otherwise.
    runLayout :: W.Workspace i (layout a) a
              -> Rectangle
              -> IO ([(a, Rectangle)], Maybe (layout a))
    runLayout (W.Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

    -- | Given a Rectangle and a Stack, return window positions.
    doLayout :: layout a -> Rectangle -> W.Stack a
             -> IO ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s = return (pureLayout l r s, Nothing)

    -- | Pure version of doLayout for layouts that don't need IO.
    pureLayout :: layout a -> Rectangle -> W.Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(W.focus s, r)]

    -- | Called when there are no windows.
    emptyLayout :: layout a -> Rectangle -> IO ([(a, Rectangle)], Maybe (layout a))
    emptyLayout _ _ = return ([], Nothing)

    -- | Handle a message. Return Nothing if the layout did not respond.
    handleMessage :: layout a -> SomeMessage -> IO (Maybe (layout a))
    handleMessage l = return . pureMessage l

    -- | Pure version of handleMessage.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _ = Nothing

    -- | Human-readable layout description.
    description :: layout a -> String
    description = show

-----------------------------------------------------------------------------
-- Existential Layout Wrapper
-----------------------------------------------------------------------------

-- | An existential type that can hold any layout.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

-- | Parse existentially wrapped layouts from a String.
readsLayout :: Layout a -> String -> [(Layout a, String)]
readsLayout (Layout l) s = [(Layout (asTypeOf x l), rs) | (x, rs) <- reads s]

instance LayoutClass Layout a where
    runLayout (W.Workspace i (Layout l) ms) r =
        fmap (fmap Layout) `fmap` runLayout (W.Workspace i l ms) r
    doLayout (Layout l) r s = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l) = description l

instance Show (Layout a) where
    show (Layout l) = show l

-----------------------------------------------------------------------------
-- Full Layout
-----------------------------------------------------------------------------

-- | Simple fullscreen mode. Renders the focused window fullscreen.
data Full a = Full
    deriving (Show, Read, Eq)

instance LayoutClass Full a where
    pureLayout Full r s = [(W.focus s, r)]
    description _ = "Full"

-----------------------------------------------------------------------------
-- Tall Layout
-----------------------------------------------------------------------------

-- | The builtin tiling mode. Supports 'Shrink', 'Expand' and 'IncMasterN'.
data Tall a = Tall
    { tallNMaster        :: !Int       -- ^ Number of windows in master pane
    , tallRatioIncrement :: !Rational  -- ^ Percent to increment when resizing
    , tallRatio          :: !Rational  -- ^ Proportion occupied by master pane
    }
    deriving (Show, Read, Eq)

instance LayoutClass Tall a where
    pureLayout (Tall nmaster _ frac) r s
        | frac == 0 = drop nmaster layout
        | frac == 1 = take nmaster layout
        | otherwise = layout
        where
            ws = W.integrate s
            rs = tile frac r nmaster (length ws)
            layout = zip ws rs

    pureMessage (Tall nmaster delta frac) m =
        msum [ fmap resize     (fromMessage m)
             , fmap incmastern (fromMessage m)
             ]
        where
            resize Shrink             = Tall nmaster delta (max 0 $ frac - delta)
            resize Expand             = Tall nmaster delta (min 1 $ frac + delta)
            incmastern (IncMasterN d) = Tall (max 0 (nmaster + d)) delta frac

    description _ = "Tall"

-- | Compute window positions using the default two-pane tiling algorithm.
tile :: Rational  -- ^ Proportion for master area
     -> Rectangle -- ^ Screen rectangle
     -> Int       -- ^ Number of windows in master pane
     -> Int       -- ^ Total number of windows
     -> [Rectangle]
tile f r nmaster n
    | n <= nmaster || nmaster == 0 = splitVertically n r
    | otherwise = splitVertically nmaster r1 ++ splitVertically (n - nmaster) r2
    where (r1, r2) = splitHorizontallyBy f r

-----------------------------------------------------------------------------
-- Mirror Layout
-----------------------------------------------------------------------------

-- | Mirror a layout, computing its 90 degree rotated form.
newtype Mirror l a = Mirror (l a)
    deriving (Show, Read, Eq)

instance LayoutClass l a => LayoutClass (Mirror l) a where
    runLayout (W.Workspace i (Mirror l) ms) r =
        (map (second mirrorRect) *** fmap Mirror)
            `fmap` runLayout (W.Workspace i l ms) (mirrorRect r)
    handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
    description (Mirror l) = "Mirror " ++ description l

-- | Mirror a rectangle (swap x/y and width/height).
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = Rectangle (fromIntegral ry) (fromIntegral rx) rh rw

-----------------------------------------------------------------------------
-- Choose Layout Combinator
-----------------------------------------------------------------------------

-- | The layout choice combinator.
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose CL
infixr 5 |||

-- | A layout that allows switching between layout options.
data Choose l r a = Choose CLR (l a) (r a)
    deriving (Read, Show, Eq)

-- | Choose the current sub-layout in 'Choose'.
data CLR = CL | CR
    deriving (Read, Show, Eq)

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
    runLayout (W.Workspace i (Choose CL l r) ms) =
        fmap (second . fmap $ flip (Choose CL) r) . runLayout (W.Workspace i l ms)
    runLayout (W.Workspace i (Choose CR l r) ms) =
        fmap (second . fmap $ Choose CR l) . runLayout (W.Workspace i r ms)

    description (Choose CL l _) = description l
    description (Choose CR _ r) = description r

    handleMessage lr m | Just NextLayout <- fromMessage m = do
        mlr' <- handleWith lr NextNoWrap
        maybe (handleWith lr FirstLayout) (return . Just) mlr'

    handleMessage c@(Choose d l r) m | Just NextNoWrap <- fromMessage m =
        case d of
            CL -> do
                ml <- handleWith l NextNoWrap
                case ml of
                    Just _  -> chooseIO c CL ml Nothing
                    Nothing -> chooseIO c CR Nothing =<< handleWith r FirstLayout
            CR -> chooseIO c CR Nothing =<< handleWith r NextNoWrap

    handleMessage c@(Choose _ l _) m | Just FirstLayout <- fromMessage m =
        flip (chooseIO c CL) Nothing =<< handleWith l FirstLayout

    handleMessage c@(Choose d l r) m | Just ReleaseResources <- fromMessage m =
        join $ liftM2 (chooseIO c d) (handleWith l ReleaseResources) (handleWith r ReleaseResources)

    handleMessage c@(Choose d l r) m | Just (JumpToLayout desc) <- fromMessage m = do
        ml <- handleMessage l m
        mr <- handleMessage r m
        let md | desc == description (fromMaybe l ml) = CL
               | desc == description (fromMaybe r mr) = CR
               | otherwise = d
        chooseIO c md ml mr

    handleMessage c@(Choose d l r) m = do
        ml' <- case d of
                CL -> handleMessage l m
                CR -> return Nothing
        mr' <- case d of
                CL -> return Nothing
                CR -> handleMessage r m
        chooseIO c d ml' mr'

-- | Helper for handleMessage.
handleWith :: (LayoutClass l a, Message m) => l a -> m -> IO (Maybe (l a))
handleWith l m = handleMessage l (SomeMessage m)

-- | Smart constructor for Choose.
chooseIO :: (LayoutClass l a, LayoutClass r a)
         => Choose l r a -> CLR -> Maybe (l a) -> Maybe (r a) -> IO (Maybe (Choose l r a))
chooseIO (Choose d _ _) d' Nothing Nothing | d == d' = return Nothing
chooseIO (Choose d l r) d' ml mr = f lr
    where
        (l', r') = (fromMaybe l ml, fromMaybe r mr)
        lr = case (d, d') of
                (CL, CR) -> (hide l' , return r')
                (CR, CL) -> (return l', hide r' )
                (_ , _ ) -> (return l', return r')
        f (x, y) = Just <$> liftM2 (Choose d') x y
        hide x = fromMaybe x <$> handleWith x Hide

-----------------------------------------------------------------------------
-- Geometry Utilities
-----------------------------------------------------------------------------

-- | Divide a rectangle vertically into n subrectangles.
splitVertically :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) =
    Rectangle sx sy sw smallh :
    splitVertically (n - 1) (Rectangle sx (sy + fromIntegral smallh) sw (sh - smallh))
    where smallh = sh `div` fromIntegral n

-- | Divide a rectangle horizontally into n subrectangles.
splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

-- | Divide a rectangle horizontally by a ratio.
splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw - fromIntegral leftw) sh
    )
    where leftw = floor $ fromIntegral sw * f

-- | Divide a rectangle vertically by a ratio.
splitVerticallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitVerticallyBy f = (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect
