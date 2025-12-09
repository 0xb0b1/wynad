-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Protocols.LayerShell
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- wlr-layer-shell protocol implementation.
-- This protocol is used for panels, status bars, backgrounds, and overlays.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Protocols.LayerShell
    ( -- * Layer Types
      Layer(..)
    , LayerSurface(..)

      -- * Layer Surface Handling
    , handleNewLayerSurface
    , handleLayerSurfaceDestroy
    , handleLayerSurfaceMap
    , handleLayerSurfaceUnmap

      -- * Layer Surface Configuration
    , configureLayerSurface
    , arrangeLayerSurfaces
    ) where

import Wynad.Core
import Wynad.Wayland.Types

-----------------------------------------------------------------------------
-- Layer Types
-----------------------------------------------------------------------------

-- | The layer a surface belongs to.
-- Surfaces in higher layers are rendered on top of lower layers.
data Layer
    = LayerBackground  -- ^ Desktop background
    | LayerBottom      -- ^ Below windows (e.g., desktop widgets)
    | LayerTop         -- ^ Above windows (e.g., panels, docks)
    | LayerOverlay     -- ^ Above everything (e.g., screen locks, notifications)
    deriving (Show, Eq, Ord, Enum)

-- | A layer shell surface.
data LayerSurface = LayerSurface
    { lsId        :: !SurfaceId
    , lsLayer     :: !Layer
    , lsOutput    :: !(Maybe OutputId)  -- ^ Preferred output, or any
    , lsAnchor    :: !Int               -- ^ Edge anchors (top, bottom, left, right)
    , lsExclusive :: !Int               -- ^ Exclusive zone (space to reserve)
    , lsMargin    :: !(Int, Int, Int, Int)  -- ^ Margins (top, right, bottom, left)
    }
    deriving (Show, Eq)

-----------------------------------------------------------------------------
-- Layer Surface Handling
-----------------------------------------------------------------------------

-- | Handle a new layer surface being created.
handleNewLayerSurface :: LayerSurface -> W ()
handleNewLayerSurface ls = do
    -- TODO: With wlhs bindings:
    -- 1. Create scene tree in appropriate layer
    -- 2. Set up map/unmap/commit listeners
    -- 3. Store layer surface state
    trace $ "New layer surface: " ++ show (lsId ls) ++ " on layer " ++ show (lsLayer ls)

-- | Handle a layer surface being destroyed.
handleLayerSurfaceDestroy :: SurfaceId -> W ()
handleLayerSurfaceDestroy sid = do
    -- TODO: With wlhs bindings:
    -- 1. Remove from layer tracking
    -- 2. Destroy scene tree
    -- 3. Rearrange remaining layer surfaces
    trace $ "Layer surface destroyed: " ++ show sid

-- | Handle a layer surface being mapped.
handleLayerSurfaceMap :: SurfaceId -> W ()
handleLayerSurfaceMap sid = do
    -- Arrange layer surfaces to account for new surface
    -- arrangeLayerSurfaces
    trace $ "Layer surface mapped: " ++ show sid

-- | Handle a layer surface being unmapped.
handleLayerSurfaceUnmap :: SurfaceId -> W ()
handleLayerSurfaceUnmap sid = do
    -- Rearrange to reclaim exclusive zone
    -- arrangeLayerSurfaces
    trace $ "Layer surface unmapped: " ++ show sid

-----------------------------------------------------------------------------
-- Layer Surface Configuration
-----------------------------------------------------------------------------

-- | Configure a layer surface with its geometry.
configureLayerSurface :: SurfaceId -> Int -> Int -> W ()
configureLayerSurface sid width height = do
    -- TODO: With wlhs bindings:
    -- wlr_layer_surface_v1_configure layerSurface width height
    return ()

-- | Arrange all layer surfaces on an output.
-- This calculates positions based on anchors, margins, and exclusive zones.
-- The exclusive zones affect the usable area for windows.
arrangeLayerSurfaces :: OutputId -> W Rectangle
arrangeLayerSurfaces oid = do
    -- TODO: With wlhs bindings:
    -- 1. Get output geometry
    -- 2. For each layer (background to overlay):
    --    - For each surface anchored to edges:
    --      - Calculate position based on anchors and margins
    --      - Reserve exclusive zone
    --      - Configure surface
    -- 3. Return usable area (after exclusive zones)

    -- Placeholder: return full output as usable
    return (Rectangle 0 0 1920 1080)
