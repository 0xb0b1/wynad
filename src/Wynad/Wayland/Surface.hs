-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Surface
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Surface (window) management for Wayland.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Surface
    ( -- * Surface Lifecycle
      handleNewSurface
    , handleSurfaceMap
    , handleSurfaceUnmap
    , handleSurfaceDestroy

      -- * Surface Operations
    , positionSurface
    , resizeSurface
    , focusSurface
    , closeSurface

      -- * Surface Properties
    , getSurfaceTitle
    , getSurfaceAppId
    , getSurfaceGeometry
    ) where

import Wynad.Core
import Wynad.Operations (manage, unmanage)
import Wynad.Wayland.Types

import Control.Monad (when)
import Data.IORef

-----------------------------------------------------------------------------
-- Surface Lifecycle
-----------------------------------------------------------------------------

-- | Handle a new XDG surface being created.
handleNewSurface :: SurfaceId -> W ()
handleNewSurface sid = do
    -- TODO: With wlhs bindings:
    -- 1. Create scene tree for the surface
    -- 2. Set up map/unmap/destroy listeners
    trace $ "New surface created: " ++ show sid

-- | Handle a surface being mapped (ready to display).
handleSurfaceMap :: SurfaceId -> W ()
handleSurfaceMap sid = do
    -- Add to window management
    manage sid
    trace $ "Surface mapped: " ++ show sid

-- | Handle a surface being unmapped (hidden).
handleSurfaceUnmap :: SurfaceId -> W ()
handleSurfaceUnmap sid = do
    -- Remove from window management
    unmanage sid
    trace $ "Surface unmapped: " ++ show sid

-- | Handle a surface being destroyed.
handleSurfaceDestroy :: SurfaceId -> W ()
handleSurfaceDestroy sid = do
    -- Ensure removed from window management
    unmanage sid
    trace $ "Surface destroyed: " ++ show sid

-----------------------------------------------------------------------------
-- Surface Operations
-----------------------------------------------------------------------------

-- | Position a surface at the given coordinates.
positionSurface :: SurfaceId -> Int -> Int -> W ()
positionSurface sid x y = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_set_position sceneNode x y
    return ()

-- | Resize a surface.
resizeSurface :: SurfaceId -> Int -> Int -> W ()
resizeSurface sid width height = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_set_size toplevel width height
    return ()

-- | Give keyboard focus to a surface.
focusSurface :: SurfaceId -> W ()
focusSurface sid = do
    -- TODO: With wlhs bindings:
    -- 1. Get the wlr_surface from toplevel
    -- 2. wlr_seat_keyboard_notify_enter seat surface
    return ()

-- | Request a surface to close.
closeSurface :: SurfaceId -> W ()
closeSurface sid = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_send_close toplevel
    trace $ "Requesting surface close: " ++ show sid

-----------------------------------------------------------------------------
-- Surface Properties
-----------------------------------------------------------------------------

-- | Get the title of a surface.
getSurfaceTitle :: SurfaceId -> W String
getSurfaceTitle sid = do
    -- TODO: With wlhs bindings:
    -- Get title from wlr_xdg_toplevel
    return ""

-- | Get the app_id of a surface.
getSurfaceAppId :: SurfaceId -> W String
getSurfaceAppId sid = do
    -- TODO: With wlhs bindings:
    -- Get app_id from wlr_xdg_toplevel
    return ""

-- | Get the geometry of a surface.
getSurfaceGeometry :: SurfaceId -> W Rectangle
getSurfaceGeometry sid = do
    -- TODO: With wlhs bindings:
    -- Get geometry from wlr_xdg_surface_get_geometry
    return (Rectangle 0 0 100 100)
