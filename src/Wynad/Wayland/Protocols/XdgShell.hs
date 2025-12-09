-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Protocols.XdgShell
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- XDG Shell protocol implementation.
-- This is the primary protocol for managing application windows in Wayland.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Protocols.XdgShell
    ( -- * XDG Surface Handling
      handleXdgNewSurface
    , handleXdgSurfaceDestroy

      -- * XDG Toplevel Operations
    , setToplevelSize
    , setToplevelActivated
    , setToplevelMaximized
    , setToplevelFullscreen
    , setToplevelTiled
    , closeToplevel

      -- * XDG Popup Operations
    , handlePopupMap
    , handlePopupUnmap
    ) where

import Wynad.Core
import Wynad.Operations (manage, unmanage)
import Wynad.Wayland.Types
import Wynad.Wayland.Surface (handleSurfaceMap, handleSurfaceUnmap)

-----------------------------------------------------------------------------
-- XDG Surface Handling
-----------------------------------------------------------------------------

-- | Handle a new XDG surface being created.
-- This is called for both toplevels (windows) and popups (menus).
handleXdgNewSurface :: SurfaceId -> Bool -> W ()
handleXdgNewSurface sid isToplevel = do
    if isToplevel
        then do
            -- Set up toplevel listeners
            trace $ "New XDG toplevel: " ++ show sid
            -- The surface will be managed when it's mapped
        else do
            -- Set up popup listeners
            trace $ "New XDG popup: " ++ show sid

-- | Handle an XDG surface being destroyed.
handleXdgSurfaceDestroy :: SurfaceId -> W ()
handleXdgSurfaceDestroy sid = do
    -- Ensure the surface is unmanaged
    unmanage sid
    trace $ "XDG surface destroyed: " ++ show sid

-----------------------------------------------------------------------------
-- XDG Toplevel Operations
-----------------------------------------------------------------------------

-- | Set the size of a toplevel window.
-- This sends a configure event to the client.
setToplevelSize :: SurfaceId -> Int -> Int -> W ()
setToplevelSize sid width height = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_set_size toplevel width height
    return ()

-- | Set whether a toplevel is activated (focused).
setToplevelActivated :: SurfaceId -> Bool -> W ()
setToplevelActivated sid activated = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_set_activated toplevel activated
    return ()

-- | Set whether a toplevel is maximized.
setToplevelMaximized :: SurfaceId -> Bool -> W ()
setToplevelMaximized sid maximized = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_set_maximized toplevel maximized
    return ()

-- | Set whether a toplevel is fullscreen.
setToplevelFullscreen :: SurfaceId -> Bool -> W ()
setToplevelFullscreen sid fullscreen = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_set_fullscreen toplevel fullscreen
    return ()

-- | Set the tiled edges of a toplevel.
-- This tells the client which edges are tiled (adjacent to other windows).
setToplevelTiled :: SurfaceId -> Bool -> Bool -> Bool -> Bool -> W ()
setToplevelTiled sid top bottom left right = do
    -- TODO: With wlhs bindings:
    -- Combine edge flags and call wlr_xdg_toplevel_set_tiled toplevel edges
    return ()

-- | Request a toplevel to close.
closeToplevel :: SurfaceId -> W ()
closeToplevel sid = do
    -- TODO: With wlhs bindings:
    -- wlr_xdg_toplevel_send_close toplevel
    trace $ "Requesting toplevel close: " ++ show sid

-----------------------------------------------------------------------------
-- XDG Popup Operations
-----------------------------------------------------------------------------

-- | Handle a popup being mapped (shown).
handlePopupMap :: SurfaceId -> W ()
handlePopupMap sid = do
    -- Popups don't go through the normal window management
    -- They're positioned relative to their parent
    trace $ "Popup mapped: " ++ show sid

-- | Handle a popup being unmapped (hidden).
handlePopupUnmap :: SurfaceId -> W ()
handlePopupUnmap sid = do
    trace $ "Popup unmapped: " ++ show sid
