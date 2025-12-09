{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Types
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Core types for the Wayland backend. These are platform-specific types
-- that wrap wlroots/Wayland primitives.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Types
    ( -- * Surface Types
      SurfaceId(..)
    , Surface(..)
      -- * Output Types
    , OutputId(..)
    , Output(..)
      -- * Geometry Types
    , Rectangle(..)
    , RationalRect(..)
      -- * Color Types
    , Color(..)
    , colorToARGB
    , rgbColor
    , rgbaColor
      -- * Input Types
    , Modifiers(..)
    , Keysym(..)
    , Keycode(..)
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
    ) where

import Data.Word (Word32, Word64)
import Data.Int (Int32)
import Data.Bits (Bits, (.|.))
import Foreign.Ptr (Ptr)

-----------------------------------------------------------------------------
-- Surface Types
-----------------------------------------------------------------------------

-- | Unique identifier for a Wayland surface (window).
-- This is used as the window type in StackSet.
newtype SurfaceId = SurfaceId Word64
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord)

-- | A managed Wayland surface.
-- This tracks the wlroots XDG toplevel and its scene tree node.
data Surface = Surface
    { surfaceId       :: !SurfaceId
    -- TODO: Add wlroots pointers when wlhs bindings are integrated
    -- , xdgToplevel   :: !(Ptr WlrXdgToplevel)
    -- , sceneTree     :: !(Ptr WlrSceneTree)
    , surfaceMapped   :: !Bool
    , surfaceTitle    :: !String
    , surfaceAppId    :: !String
    }
    deriving (Show, Eq)

-----------------------------------------------------------------------------
-- Output Types
-----------------------------------------------------------------------------

-- | Unique identifier for a Wayland output (monitor).
newtype OutputId = OutputId Word32
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Enum, Num, Integral, Real)

-- | A Wayland output (monitor/display).
data Output = Output
    { outputId        :: !OutputId
    , outputName      :: !String
    , outputRect      :: !Rectangle
    , outputScale     :: !Int32
    , outputEnabled   :: !Bool
    }
    deriving (Show, Eq)

-----------------------------------------------------------------------------
-- Geometry Types
-----------------------------------------------------------------------------

-- | A rectangle with position and dimensions.
-- Used for window and output geometry.
data Rectangle = Rectangle
    { rect_x      :: !Int32
    , rect_y      :: !Int32
    , rect_width  :: !Word32
    , rect_height :: !Word32
    }
    deriving (Show, Read, Eq)

-- | A rectangle with rational coordinates, for floating windows.
-- Values are relative to screen size (0.0 to 1.0).
data RationalRect = RationalRect
    { rr_x      :: !Rational
    , rr_y      :: !Rational
    , rr_width  :: !Rational
    , rr_height :: !Rational
    }
    deriving (Show, Read, Eq)

-----------------------------------------------------------------------------
-- Color Types
-----------------------------------------------------------------------------

-- | ARGB color value.
newtype Color = Color Word32
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord)

-- | Convert Color to ARGB components (alpha, red, green, blue).
colorToARGB :: Color -> (Word32, Word32, Word32, Word32)
colorToARGB (Color c) =
    ( (c `div` 0x1000000) `mod` 0x100
    , (c `div` 0x10000) `mod` 0x100
    , (c `div` 0x100) `mod` 0x100
    , c `mod` 0x100
    )

-- | Create a color from RGB values (0-255). Alpha is set to 255 (opaque).
rgbColor :: Word32 -> Word32 -> Word32 -> Color
rgbColor r g b = Color (0xFF000000 .|. (r * 0x10000) .|. (g * 0x100) .|. b)

-- | Create a color from RGBA values (0-255).
rgbaColor :: Word32 -> Word32 -> Word32 -> Word32 -> Color
rgbaColor r g b a = Color ((a * 0x1000000) .|. (r * 0x10000) .|. (g * 0x100) .|. b)

-----------------------------------------------------------------------------
-- Input Types
-----------------------------------------------------------------------------

-- | Keyboard modifiers (Shift, Control, Alt, Super, etc.)
newtype Modifiers = Modifiers Word32
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Bits, Num)

-- | XKB keysym (symbolic key identifier).
newtype Keysym = Keysym Word32
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Num)

-- | Physical keycode (evdev).
newtype Keycode = Keycode Word32
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Num)

-- | Mouse button identifier.
newtype Button = Button Word32
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Num)

-----------------------------------------------------------------------------
-- Modifier Constants
-- These correspond to XKB/evdev modifier bits
-----------------------------------------------------------------------------

noModifier :: Modifiers
noModifier = Modifiers 0

shiftMask :: Modifiers
shiftMask = Modifiers 0x01

controlMask :: Modifiers
controlMask = Modifiers 0x04

altMask :: Modifiers
altMask = Modifiers 0x08

mod1Mask :: Modifiers
mod1Mask = altMask

mod4Mask :: Modifiers
mod4Mask = Modifiers 0x40

superMask :: Modifiers
superMask = mod4Mask

-----------------------------------------------------------------------------
-- Button Constants
-- These correspond to evdev button codes
-----------------------------------------------------------------------------

-- | Left mouse button
button1 :: Button
button1 = Button 0x110  -- BTN_LEFT

-- | Right mouse button
button2 :: Button
button2 = Button 0x111  -- BTN_RIGHT

-- | Middle mouse button
button3 :: Button
button3 = Button 0x112  -- BTN_MIDDLE
