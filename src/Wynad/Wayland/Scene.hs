-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Scene
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Scene graph operations for Wayland rendering.
-- The scene graph is wlroots' abstraction for compositing and rendering.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Scene
    ( -- * Scene Operations
      createSceneTree
    , destroySceneTree

      -- * Node Operations
    , setNodePosition
    , setNodeEnabled
    , raiseNode
    , lowerNode

      -- * Surface Operations
    , attachSurfaceToScene
    , detachSurfaceFromScene

      -- * Hit Testing
    , nodeAt
    , surfaceAt
    ) where

import Wynad.Core
import Wynad.Wayland.Types

-----------------------------------------------------------------------------
-- Scene Operations
-----------------------------------------------------------------------------

-- | Create a new scene tree for a surface.
createSceneTree :: SurfaceId -> W ()
createSceneTree sid = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_tree_create sceneRoot
    return ()

-- | Destroy a scene tree.
destroySceneTree :: SurfaceId -> W ()
destroySceneTree sid = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_destroy sceneNode
    return ()

-----------------------------------------------------------------------------
-- Node Operations
-----------------------------------------------------------------------------

-- | Set the position of a scene node.
setNodePosition :: SurfaceId -> Int -> Int -> W ()
setNodePosition sid x y = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_set_position node x y
    return ()

-- | Enable or disable a scene node (visibility).
setNodeEnabled :: SurfaceId -> Bool -> W ()
setNodeEnabled sid enabled = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_set_enabled node enabled
    return ()

-- | Raise a node to the top of the stacking order.
raiseNode :: SurfaceId -> W ()
raiseNode sid = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_raise_to_top node
    return ()

-- | Lower a node to the bottom of the stacking order.
lowerNode :: SurfaceId -> W ()
lowerNode sid = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_lower_to_bottom node
    return ()

-----------------------------------------------------------------------------
-- Surface Operations
-----------------------------------------------------------------------------

-- | Attach a Wayland surface to the scene graph.
attachSurfaceToScene :: SurfaceId -> W ()
attachSurfaceToScene sid = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_xdg_surface_create parentTree xdgSurface
    return ()

-- | Detach a surface from the scene graph.
detachSurfaceFromScene :: SurfaceId -> W ()
detachSurfaceFromScene sid = do
    -- Destroying the scene tree removes it from the graph
    destroySceneTree sid

-----------------------------------------------------------------------------
-- Hit Testing
-----------------------------------------------------------------------------

-- | Find the scene node at a given position.
nodeAt :: Double -> Double -> W (Maybe SurfaceId)
nodeAt x y = do
    -- TODO: With wlhs bindings:
    -- wlr_scene_node_at sceneRoot x y &localX &localY
    return Nothing

-- | Find the surface at a given position, returning local coordinates.
surfaceAt :: Double -> Double -> W (Maybe (SurfaceId, Double, Double))
surfaceAt x y = do
    -- TODO: With wlhs bindings:
    -- 1. Find node at position
    -- 2. Get the surface from the node
    -- 3. Return surface and local coordinates
    return Nothing
