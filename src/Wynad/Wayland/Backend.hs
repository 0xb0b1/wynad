{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Backend
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Wayland compositor backend using wlroots via wlhs bindings.
--
-- This module handles:
-- - Initializing the Wayland display and wlroots backend
-- - Setting up the scene graph for rendering
-- - Managing the event loop
-- - Coordinating inputs and outputs
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Backend
    ( -- * Backend Type
      WaylandBackend(..)

      -- * Initialization
    , initBackend
    , destroyBackend

      -- * Event Loop
    , runEventLoop

      -- * Launch Function
    , launch
    ) where

import Wynad.Core
import Wynad.Layout
import Wynad.Wayland.Types
import qualified Wynad.StackSet as W

import Control.Monad (when, unless, forever)
import Data.IORef
import Foreign.Ptr (Ptr, nullPtr)

import qualified Data.Map as M
import qualified Data.Set as S

-----------------------------------------------------------------------------
-- Backend Type
-----------------------------------------------------------------------------

-- | The Wayland compositor backend.
-- This holds all wlroots state needed to run the compositor.
data WaylandBackend = WaylandBackend
    { wbSurfaces     :: !(IORef (M.Map SurfaceId Surface))
      -- ^ Map of managed surfaces
    , wbOutputs      :: !(IORef [Output])
      -- ^ List of connected outputs
    , wbNextSurfaceId :: !(IORef Word64)
      -- ^ Counter for generating unique surface IDs
    , wbNextOutputId  :: !(IORef Word32)
      -- ^ Counter for generating unique output IDs

    -- TODO: Add wlroots pointers when wlhs bindings are integrated
    -- , wlDisplay      :: !(Ptr WlDisplay)
    -- , wlrBackend     :: !(Ptr WlrBackend)
    -- , wlrRenderer    :: !(Ptr WlrRenderer)
    -- , wlrAllocator   :: !(Ptr WlrAllocator)
    -- , wlrScene       :: !(Ptr WlrScene)
    -- , wlrSceneLayout :: !(Ptr WlrSceneOutputLayout)
    -- , wlrOutputLayout :: !(Ptr WlrOutputLayout)
    -- , wlrSeat        :: !(Ptr WlrSeat)
    -- , wlrXdgShell    :: !(Ptr WlrXdgShell)
    }

import Data.Word (Word64, Word32)

-----------------------------------------------------------------------------
-- Initialization
-----------------------------------------------------------------------------

-- | Initialize the Wayland backend.
-- This creates the Wayland display, wlroots backend, and all necessary
-- protocols and extensions.
initBackend :: IO WaylandBackend
initBackend = do
    -- Initialize IORef state
    surfaces <- newIORef M.empty
    outputs <- newIORef []
    nextSurfaceId <- newIORef 1
    nextOutputId <- newIORef 0

    -- TODO: Initialize wlroots when wlhs bindings are available
    -- wlDisplay <- wl_display_create
    -- backend <- wlr_backend_autocreate wlDisplay Nothing
    -- renderer <- wlr_renderer_autocreate backend
    -- wlr_renderer_init_wl_display renderer wlDisplay
    -- allocator <- wlr_allocator_autocreate backend renderer
    --
    -- scene <- wlr_scene_create
    -- outputLayout <- wlr_output_layout_create wlDisplay
    -- sceneLayout <- wlr_scene_attach_output_layout scene outputLayout
    --
    -- wlr_compositor_create wlDisplay 6 renderer
    -- wlr_subcompositor_create wlDisplay
    -- wlr_data_device_manager_create wlDisplay
    --
    -- xdgShell <- wlr_xdg_shell_create wlDisplay 3
    -- seat <- wlr_seat_create wlDisplay "seat0"
    --
    -- Setup signal handlers for new_output, new_input, new_xdg_surface

    return WaylandBackend
        { wbSurfaces = surfaces
        , wbOutputs = outputs
        , wbNextSurfaceId = nextSurfaceId
        , wbNextOutputId = nextOutputId
        }

-- | Destroy the Wayland backend and clean up resources.
destroyBackend :: WaylandBackend -> IO ()
destroyBackend WaylandBackend{..} = do
    -- TODO: Cleanup wlroots resources
    -- wlr_backend_destroy wlrBackend
    -- wl_display_destroy wlDisplay
    return ()

-----------------------------------------------------------------------------
-- Event Loop
-----------------------------------------------------------------------------

-- | Run the Wayland event loop.
-- This starts the backend and runs until shutdown.
runEventLoop :: WaylandBackend -> IO ()
runEventLoop WaylandBackend{..} = do
    -- TODO: Start wlroots backend and run display
    -- wlr_backend_start wlrBackend
    -- wl_display_run wlDisplay

    -- Placeholder: just wait
    putStrLn "Wynad: Backend event loop not yet implemented"
    putStrLn "Wynad: Waiting for wlhs bindings integration..."

    -- In the real implementation, this would be:
    -- forever $ do
    --     event <- nextEvent
    --     handleEvent event

-----------------------------------------------------------------------------
-- Launch Function
-----------------------------------------------------------------------------

-- | Main entry point for launching Wynad.
-- This initializes the backend, sets up the initial state, and runs the event loop.
launch :: LayoutClass l SurfaceId => WConfig l -> IO ()
launch cfg = do
    putStrLn "Starting Wynad..."

    -- Initialize the backend
    backend <- initBackend
    putStrLn "Backend initialized"

    -- Wait for outputs to be connected
    outputs <- readIORef (wbOutputs backend)
    let screenDetails = map outputToScreenDetail outputs

    -- Create initial window set
    let layoutHook' = Layout (layoutHook cfg)
        initialWindowSet = W.new layoutHook' (workspaces cfg) screenDetails

    -- Create initial state
    let initialState = WState
            { windowset = initialWindowSet
            , mapped = S.empty
            , waitingUnmap = M.empty
            , dragging = Nothing
            , extensibleState = M.empty
            }

    -- Create configuration
    let conf = WConf
            { config = cfg { layoutHook = layoutHook' }
            , normalBorder = normalBorderColor cfg
            , focusedBorder = focusedBorderColor cfg
            , keyActions = keys cfg (cfg { layoutHook = layoutHook' })
            , buttonActions = mouseBindings cfg (cfg { layoutHook = layoutHook' })
            , currentEvent = Nothing
            }

    -- Run startup hook
    (_, state') <- runW conf initialState (startupHook cfg)
    putStrLn "Startup hook completed"

    -- Run the event loop
    runEventLoop backend

    -- Cleanup
    destroyBackend backend
    putStrLn "Wynad exited"

-- | Convert an Output to ScreenDetail.
outputToScreenDetail :: Output -> ScreenDetail
outputToScreenDetail = SD . outputRect

-----------------------------------------------------------------------------
-- TODO: Signal Handlers (to be implemented with wlhs)
-----------------------------------------------------------------------------

-- handleNewOutput :: Ptr WlrOutput -> IO ()
-- handleNewOutput output = do
--     -- Configure output
--     -- Add to output layout
--     -- Create scene output
--     -- Emit output ready

-- handleNewInput :: Ptr WlrInputDevice -> IO ()
-- handleNewInput device = do
--     -- Add keyboard or pointer to seat
--     -- Set up key/button handlers

-- handleNewXdgSurface :: Ptr WlrXdgSurface -> IO ()
-- handleNewXdgSurface surface = do
--     -- Check if toplevel
--     -- Create surface tracking
--     -- Set up map/unmap/destroy listeners
--     -- Call manage() when mapped
