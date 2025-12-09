-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Wynad: An XMonad-inspired Wayland compositor.
--
-- This is the main module that re-exports the public API.
--
-----------------------------------------------------------------------------

module Wynad
    ( -- * Main Entry Point
      wynad
    , launch

      -- * Configuration
    , WConfig(..)
    , defaultConfig

      -- * The W Monad
    , W
    , io
    , withWindowSet
    , runW

      -- * Window Management
    , manage
    , unmanage
    , windows
    , refresh
    , kill

      -- * Focus Operations
    , focus
    , focusUp
    , focusDown
    , focusMaster
    , withFocused
    , withUnfocused

      -- * Swap Operations
    , swapUp
    , swapDown
    , swapMaster
    , shiftMaster

      -- * Floating Operations
    , float
    , sink

      -- * Layout Operations
    , sendMessage
    , setLayout
    , broadcastMessage

      -- * Screen Operations
    , screenWorkspace

      -- * Spawn
    , spawn

      -- * Layouts
    , Full(..)
    , Tall(..)
    , Mirror(..)
    , Choose
    , (|||)

      -- * Layout Messages
    , Resize(..)
    , IncMasterN(..)
    , ChangeLayout(..)
    , JumpToLayout(..)

      -- * ManageHook
    , ManageHook
    , Query
    , composeAll
    , composeOne
    , (<&&>)
    , (<||>)
    , (-->)
    , (-?>)
    , title
    , appId
    , className
    , appName
    , doFloat
    , doShift
    , doIgnore
    , doSink
    , idHook

      -- * StackSet Operations
    , module Wynad.StackSet

      -- * Types
    , SurfaceId(..)
    , Rectangle(..)
    , RationalRect(..)
    , Color(..)
    , rgbColor
    , rgbaColor
    , Modifiers(..)
    , Keysym(..)
    , Button(..)

      -- * Modifier Constants
    , noModifier
    , shiftMask
    , controlMask
    , altMask
    , mod1Mask
    , mod4Mask
    , superMask

      -- * Button Constants
    , button1
    , button2
    , button3

      -- * Re-exports
    , Default(..)
    , asks
    , gets
    , when
    , unless
    , void
    , liftIO
    ) where

import Wynad.Core
import Wynad.Config
import Wynad.Layout
import Wynad.ManageHook
import Wynad.Operations
import Wynad.StackSet
import Wynad.Wayland.Types
import Wynad.Wayland.Backend (launch)

import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.Default.Class (Default(..))
import System.Posix.Process (executeFile, forkProcess)

-- | Main entry point for Wynad.
-- Call this from your configuration file.
wynad :: LayoutClass l SurfaceId => WConfig l -> IO ()
wynad = launch

-- | Spawn a command.
spawn :: String -> W ()
spawn cmd = io $ void $ forkProcess $ executeFile "/bin/sh" False ["-c", cmd] Nothing
