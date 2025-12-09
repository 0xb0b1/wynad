{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Operations
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Window management operations for Wynad.
-- These functions manipulate the window set and interact with Wayland.
--
-----------------------------------------------------------------------------

module Wynad.Operations
    ( -- * Window Management
      manage
    , unmanage
    , windows
    , refresh
    , focus
    , setFocus

      -- * Floating Operations
    , float
    , sink

      -- * Layout Operations
    , sendMessage
    , setLayout
    , broadcastMessage

      -- * Focus Operations
    , focusUp
    , focusDown
    , focusMaster
    , swapUp
    , swapDown
    , swapMaster
    , shiftMaster

      -- * Window Utilities
    , kill
    , screenWorkspace
    , withFocused
    , withUnfocused
    ) where

import Wynad.Core
import Wynad.Layout
import Wynad.Wayland.Types
import qualified Wynad.StackSet as W

import Control.Monad (when, forM_, unless)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Monoid (Endo(..))
import Data.Maybe (isNothing, fromMaybe)

import qualified Data.Map as M
import qualified Data.Set as S

-----------------------------------------------------------------------------
-- Window Management
-----------------------------------------------------------------------------

-- | Add a new surface to the window manager.
-- This is called when a new XDG toplevel surface is mapped.
manage :: SurfaceId -> W ()
manage sid = do
    -- Check if already managed
    ws <- gets windowset
    unless (W.member sid ws) $ do
        -- Run the manage hook
        mh <- asks (manageHook . config)
        g <- appEndo <$> userCodeDef (Endo id) (runQuery mh sid)

        -- Insert the window and apply the manage hook transformation
        windows (g . W.insertUp sid)

        -- Mark as mapped
        modify $ \s -> s { mapped = S.insert sid (mapped s) }

-- | Remove a surface from the window manager.
-- This is called when an XDG toplevel surface is destroyed.
unmanage :: SurfaceId -> W ()
unmanage sid = do
    -- Remove from mapped set
    modify $ \s -> s { mapped = S.delete sid (mapped s) }

    -- Remove from window set
    windows (W.delete sid)

-- | Apply a transformation to the window set and refresh the layout.
-- This is the core function for all window set modifications.
windows :: (WindowSet -> WindowSet) -> W ()
windows f = do
    -- Apply the transformation
    modify $ \s -> s { windowset = f (windowset s) }

    -- Refresh the display
    refresh

    -- Run the log hook
    userCodeDef () =<< asks (logHook . config)

-- | Recompute and apply the current layout.
-- This positions all windows according to the active layout algorithm.
refresh :: W ()
refresh = do
    ws <- gets windowset

    -- Get configuration
    bw <- asks (borderWidth . config)
    nbc <- asks normalBorder
    fbc <- asks focusedBorder

    -- Process each visible screen
    forM_ (W.screens ws) $ \scr -> do
        let wsp = W.workspace scr
            rect = screenRect (W.screenDetail scr)
            focused = W.peek ws

        -- Run the layout
        (rects, ml) <- io $ runLayout wsp rect

        -- Update layout if changed
        whenJust ml $ \l' ->
            modify $ \s -> s { windowset = updateLayout (W.tag wsp) l' (windowset s) }

        -- Position each window
        forM_ rects $ \(sid, r) -> do
            -- Check if floating
            let isFloating = M.member sid (W.floating ws)

            unless isFloating $ do
                -- Apply border width adjustment
                let r' = shrinkRect bw r

                -- Position the surface
                -- TODO: Call Wayland backend to position surface
                -- positionSurface sid r'

                -- Set border color based on focus
                let borderColor = if Just sid == focused then fbc else nbc
                -- TODO: setBorderColor sid borderColor

                return ()

    -- Handle floating windows
    forM_ (M.toList (W.floating ws)) $ \(sid, rr) -> do
        -- Convert rational rect to absolute
        let scr = W.current ws
            scrRect = screenRect (W.screenDetail scr)
            r = rationalToRect scrRect rr

        -- TODO: Position floating surface
        -- positionSurface sid r

        return ()

-- | Update the layout for a specific workspace.
updateLayout :: WorkspaceId -> Layout SurfaceId -> WindowSet -> WindowSet
updateLayout t l = W.mapWorkspace $ \ws ->
    if W.tag ws == t then ws { W.layout = l } else ws

-- | Shrink a rectangle by a border width on all sides.
shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect bw (Rectangle x y w h) = Rectangle
    (x + fromIntegral bw)
    (y + fromIntegral bw)
    (w - fromIntegral (2 * bw))
    (h - fromIntegral (2 * bw))

-- | Convert a rational rectangle to absolute coordinates.
rationalToRect :: Rectangle -> RationalRect -> Rectangle
rationalToRect (Rectangle sx sy sw sh) (RationalRect rx ry rw rh) = Rectangle
    (sx + floor (fromIntegral sw * rx))
    (sy + floor (fromIntegral sh * ry))
    (floor (fromIntegral sw * rw))
    (floor (fromIntegral sh * rh))

-----------------------------------------------------------------------------
-- Focus Operations
-----------------------------------------------------------------------------

-- | Set focus to a specific surface.
focus :: SurfaceId -> W ()
focus sid = windows (W.focusWindow sid)

-- | Set focus to a surface (same as focus, provided for compatibility).
setFocus :: SurfaceId -> W ()
setFocus = focus

-- | Move focus to the previous window.
focusUp :: W ()
focusUp = windows W.focusUp

-- | Move focus to the next window.
focusDown :: W ()
focusDown = windows W.focusDown

-- | Move focus to the master window.
focusMaster :: W ()
focusMaster = windows W.focusMaster

-----------------------------------------------------------------------------
-- Swap Operations
-----------------------------------------------------------------------------

-- | Swap the focused window with the previous one.
swapUp :: W ()
swapUp = windows W.swapUp

-- | Swap the focused window with the next one.
swapDown :: W ()
swapDown = windows W.swapDown

-- | Swap the focused window with the master window.
swapMaster :: W ()
swapMaster = windows W.swapMaster

-- | Move the focused window to the master position.
shiftMaster :: W ()
shiftMaster = windows W.shiftMaster

-----------------------------------------------------------------------------
-- Floating Operations
-----------------------------------------------------------------------------

-- | Float a window with the given rational rectangle.
float :: SurfaceId -> RationalRect -> W ()
float sid rr = windows (W.float sid rr)

-- | Sink a floating window back into tiling.
sink :: SurfaceId -> W ()
sink sid = windows (W.sink sid)

-----------------------------------------------------------------------------
-- Layout Operations
-----------------------------------------------------------------------------

-- | Send a message to the current layout.
sendMessage :: Message a => a -> W ()
sendMessage m = do
    ws <- gets windowset
    let wsp = W.workspace (W.current ws)
    ml <- io $ handleMessage (W.layout wsp) (SomeMessage m)
    whenJust ml $ \l' ->
        modify $ \s -> s { windowset = updateLayout (W.tag wsp) l' (windowset s) }
    refresh

-- | Replace the current layout.
setLayout :: Layout SurfaceId -> W ()
setLayout l = do
    ws <- gets windowset
    let wsp = W.workspace (W.current ws)
    modify $ \s -> s { windowset = updateLayout (W.tag wsp) l (windowset s) }
    refresh

-- | Broadcast a message to all layouts.
broadcastMessage :: Message a => a -> W ()
broadcastMessage m = do
    ws <- gets windowset
    forM_ (W.workspaces ws) $ \wsp -> do
        ml <- io $ handleMessage (W.layout wsp) (SomeMessage m)
        whenJust ml $ \l' ->
            modify $ \s -> s { windowset = updateLayout (W.tag wsp) l' (windowset s) }

-----------------------------------------------------------------------------
-- Window Utilities
-----------------------------------------------------------------------------

-- | Close the focused window.
kill :: W ()
kill = withFocused $ \sid -> do
    -- TODO: Send close request via XDG toplevel protocol
    -- xdgToplevelClose sid
    return ()

-- | Get the workspace visible on a given screen.
screenWorkspace :: ScreenId -> W (Maybe WorkspaceId)
screenWorkspace sc = gets (W.lookupWorkspace sc . windowset)

-- | Perform an action with the focused window.
withFocused :: (SurfaceId -> W ()) -> W ()
withFocused f = do
    mfocused <- gets (W.peek . windowset)
    whenJust mfocused f

-- | Perform an action with all unfocused windows.
withUnfocused :: (SurfaceId -> W ()) -> W ()
withUnfocused f = do
    ws <- gets windowset
    let focused = W.peek ws
        allWins = W.allWindows ws
        unfocused = filter (\w -> Just w /= focused) allWins
    mapM_ f unfocused
