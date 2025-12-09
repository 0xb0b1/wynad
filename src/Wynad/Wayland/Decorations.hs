-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Decorations
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Window decorations (borders) for Wayland.
-- Since Wayland has no built-in window decoration, we render borders
-- using scene graph primitives.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Decorations
    ( -- * Border Management
      createBorders
    , destroyBorders
    , updateBorders

      -- * Border Colors
    , setBorderColor
    , setFocusedBorder
    , setUnfocusedBorder
    ) where

import Wynad.Core
import Wynad.Wayland.Types

-----------------------------------------------------------------------------
-- Border Management
-----------------------------------------------------------------------------

-- | Create borders around a surface.
-- This creates four scene rects (top, bottom, left, right) that surround
-- the surface to form a border.
createBorders :: SurfaceId -> Int -> Color -> W ()
createBorders sid borderWidth color = do
    -- TODO: With wlhs bindings:
    -- Create 4 wlr_scene_rect nodes around the surface
    -- wlr_scene_rect_create parentTree width height color
    return ()

-- | Destroy the borders of a surface.
destroyBorders :: SurfaceId -> W ()
destroyBorders sid = do
    -- TODO: With wlhs bindings:
    -- Destroy the 4 border rect nodes
    return ()

-- | Update border positions after a surface moves or resizes.
updateBorders :: SurfaceId -> Rectangle -> Int -> W ()
updateBorders sid rect borderWidth = do
    -- TODO: With wlhs bindings:
    -- Reposition and resize the 4 border rect nodes
    let Rectangle x y w h = rect
        bw = fromIntegral borderWidth

    -- Top border: full width, at top
    -- Bottom border: full width, at bottom
    -- Left border: border height, on left
    -- Right border: border height, on right

    return ()

-----------------------------------------------------------------------------
-- Border Colors
-----------------------------------------------------------------------------

-- | Set the color of a surface's borders.
setBorderColor :: SurfaceId -> Color -> W ()
setBorderColor sid color = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_rect_set_color for each border rect
    return ()

-- | Set the border to the focused color.
setFocusedBorder :: SurfaceId -> W ()
setFocusedBorder sid = do
    color <- asks focusedBorder
    setBorderColor sid color

-- | Set the border to the unfocused color.
setUnfocusedBorder :: SurfaceId -> W ()
setUnfocusedBorder sid = do
    color <- asks normalBorder
    setBorderColor sid color
