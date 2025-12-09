-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Input
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Input device management for Wayland.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Input
    ( -- * Input Device Handling
      handleNewInput
    , handleInputDestroy

      -- * Pointer Operations
    , handlePointerMotion
    , handlePointerButton
    , handlePointerAxis

      -- * Cursor Management
    , setCursorImage
    , setCursorSurface
    , warpCursor
    ) where

import Wynad.Core
import Wynad.Wayland.Types

import Control.Monad (when)

-----------------------------------------------------------------------------
-- Input Device Handling
-----------------------------------------------------------------------------

-- | Handle a new input device being connected.
handleNewInput :: String -> W ()
handleNewInput deviceType = do
    -- TODO: With wlhs bindings:
    -- 1. Determine device type (keyboard, pointer, touch)
    -- 2. Add to appropriate seat capability
    -- 3. Set up event handlers
    trace $ "New input device: " ++ deviceType

-- | Handle an input device being disconnected.
handleInputDestroy :: String -> W ()
handleInputDestroy deviceName = do
    -- TODO: With wlhs bindings:
    -- 1. Remove from seat
    -- 2. Update capabilities
    trace $ "Input device disconnected: " ++ deviceName

-----------------------------------------------------------------------------
-- Pointer Operations
-----------------------------------------------------------------------------

-- | Handle pointer motion events.
handlePointerMotion :: Double -> Double -> W ()
handlePointerMotion x y = do
    -- TODO: With wlhs bindings:
    -- 1. Update cursor position
    -- 2. Find surface under cursor (wlr_scene_node_at)
    -- 3. Update pointer focus
    -- 4. Handle focus-follows-mouse
    return ()

-- | Handle pointer button events.
handlePointerButton :: Button -> Bool -> W ()
handlePointerButton button pressed = do
    -- TODO: With wlhs bindings:
    -- 1. Check for compositor keybindings
    -- 2. Update focus if click-to-focus
    -- 3. Forward to focused surface
    return ()

-- | Handle pointer scroll/axis events.
handlePointerAxis :: Double -> Double -> W ()
handlePointerAxis horizontal vertical = do
    -- TODO: With wlhs bindings:
    -- Forward to focused surface via seat
    return ()

-----------------------------------------------------------------------------
-- Cursor Management
-----------------------------------------------------------------------------

-- | Set the cursor image.
setCursorImage :: String -> W ()
setCursorImage name = do
    -- TODO: With wlhs bindings:
    -- wlr_cursor_set_xcursor cursor xcursorManager name
    return ()

-- | Set the cursor to a client surface.
setCursorSurface :: SurfaceId -> Int -> Int -> W ()
setCursorSurface sid hotspotX hotspotY = do
    -- TODO: With wlhs bindings:
    -- wlr_cursor_set_surface cursor surface hotspotX hotspotY
    return ()

-- | Warp the cursor to a position.
warpCursor :: Double -> Double -> W ()
warpCursor x y = do
    -- TODO: With wlhs bindings:
    -- wlr_cursor_warp_absolute cursor NULL x y
    return ()
