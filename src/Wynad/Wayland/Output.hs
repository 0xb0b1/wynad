-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Output
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Output (monitor/display) management for Wayland.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Output
    ( -- * Output Management
      handleNewOutput
    , handleOutputDestroy
    , handleOutputFrame

      -- * Output Configuration
    , configureOutput
    , setOutputMode

      -- * Output Layout
    , addOutputToLayout
    , removeOutputFromLayout
    , getOutputLayout
    ) where

import Wynad.Core
import Wynad.Wayland.Types

import Data.IORef
import Control.Monad (when)

-----------------------------------------------------------------------------
-- Output Handlers
-----------------------------------------------------------------------------

-- | Handle a new output being connected.
-- This is called when wlroots detects a new monitor.
handleNewOutput :: Output -> W ()
handleNewOutput output = do
    -- TODO: With wlhs bindings:
    -- 1. Configure the output (mode, scale, transform)
    -- 2. Add to output layout
    -- 3. Create scene output
    -- 4. Update window set with new screen
    trace $ "New output connected: " ++ outputName output

-- | Handle an output being disconnected.
handleOutputDestroy :: OutputId -> W ()
handleOutputDestroy oid = do
    -- TODO: With wlhs bindings:
    -- 1. Remove from output layout
    -- 2. Update window set (move windows to other screens)
    -- 3. Cleanup scene output
    trace $ "Output disconnected: " ++ show oid

-- | Handle an output frame event.
-- This is called when an output is ready to be rendered.
handleOutputFrame :: OutputId -> W ()
handleOutputFrame oid = do
    -- TODO: With wlhs bindings:
    -- 1. Get the scene output
    -- 2. Render the scene to the output
    -- 3. Commit the output
    return ()

-----------------------------------------------------------------------------
-- Output Configuration
-----------------------------------------------------------------------------

-- | Configure an output with default settings.
configureOutput :: Output -> IO Output
configureOutput output = do
    -- TODO: With wlhs bindings:
    -- 1. Get preferred mode
    -- 2. Set mode
    -- 3. Enable output
    -- 4. Commit configuration
    return output { outputEnabled = True }

-- | Set the output mode (resolution and refresh rate).
setOutputMode :: OutputId -> Int -> Int -> Int -> W ()
setOutputMode oid width height refreshRate = do
    -- TODO: With wlhs bindings:
    -- 1. Find matching mode
    -- 2. Set mode
    -- 3. Commit output
    trace $ "Setting output " ++ show oid ++
            " to " ++ show width ++ "x" ++ show height ++
            "@" ++ show refreshRate ++ "Hz"

-----------------------------------------------------------------------------
-- Output Layout
-----------------------------------------------------------------------------

-- | Add an output to the layout.
-- This positions the output in the virtual coordinate space.
addOutputToLayout :: Output -> W ()
addOutputToLayout output = do
    -- TODO: With wlhs bindings:
    -- wlr_output_layout_add_auto outputLayout wlrOutput
    trace $ "Adding output to layout: " ++ outputName output

-- | Remove an output from the layout.
removeOutputFromLayout :: OutputId -> W ()
removeOutputFromLayout oid = do
    -- TODO: With wlhs bindings:
    -- wlr_output_layout_remove outputLayout wlrOutput
    trace $ "Removing output from layout: " ++ show oid

-- | Get the combined geometry of all outputs.
getOutputLayout :: W [Rectangle]
getOutputLayout = do
    -- TODO: With wlhs bindings:
    -- Get all output geometries from wlr_output_layout
    return []
