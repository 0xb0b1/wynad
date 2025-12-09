{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Config
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Default configuration for Wynad.
--
-----------------------------------------------------------------------------

module Wynad.Config
    ( -- * Default Configuration
      defaultConfig

      -- * Default Bindings
    , defaultKeys
    , defaultMouseBindings

      -- * Re-exports for Configuration
    , module Wynad.Core
    , module Wynad.Layout
    , module Wynad.Wayland.Types
    ) where

import Wynad.Core
import Wynad.Layout
import Wynad.Operations
import Wynad.Wayland.Types

import qualified Wynad.StackSet as W

import Data.Bits ((.|.))
import Data.Default.Class
import System.Posix.Process (executeFile, forkProcess)
import Control.Monad (void)

import qualified Data.Map as M

-----------------------------------------------------------------------------
-- Default Configuration
-----------------------------------------------------------------------------

-- | The default configuration for Wynad.
defaultConfig :: WConfig (Choose Tall (Choose (Mirror Tall) Full))
defaultConfig = def
    { terminal           = "foot"
    , modMask            = mod4Mask
    , workspaces         = map show [1..9 :: Int]
    , borderWidth        = 2
    , normalBorderColor  = rgbColor 0x44 0x44 0x44
    , focusedBorderColor = rgbColor 0x00 0xff 0x00
    , layoutHook         = defaultLayout
    , keys               = defaultKeys
    , mouseBindings      = defaultMouseBindings
    , focusFollowsMouse  = True
    , clickJustFocuses   = True
    }

-- | The default layout configuration.
defaultLayout :: Choose Tall (Choose (Mirror Tall) Full) SurfaceId
defaultLayout = Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2)) ||| Full

-----------------------------------------------------------------------------
-- Default Key Bindings
-----------------------------------------------------------------------------

-- | Default key bindings.
-- These are modeled after XMonad's defaults.
defaultKeys :: WConfig Layout -> M.Map (Modifiers, Keysym) (W ())
defaultKeys conf = M.fromList $
    -- Launch terminal
    [ ((modMask conf .|. shiftMask, xK_Return), spawn $ terminal conf)

    -- Close focused window
    , ((modMask conf .|. shiftMask, xK_c), kill)

    -- Rotate through layouts
    , ((modMask conf, xK_space), sendMessage NextLayout)

    -- Reset layout to default
    , ((modMask conf .|. shiftMask, xK_space), setLayout (Layout defaultLayout))

    -- Move focus
    , ((modMask conf, xK_Tab), focusDown)
    , ((modMask conf .|. shiftMask, xK_Tab), focusUp)
    , ((modMask conf, xK_j), focusDown)
    , ((modMask conf, xK_k), focusUp)
    , ((modMask conf, xK_m), focusMaster)

    -- Swap focused window
    , ((modMask conf .|. shiftMask, xK_j), swapDown)
    , ((modMask conf .|. shiftMask, xK_k), swapUp)
    , ((modMask conf, xK_Return), swapMaster)

    -- Resize master area
    , ((modMask conf, xK_h), sendMessage Shrink)
    , ((modMask conf, xK_l), sendMessage Expand)

    -- Increment/decrement master count
    , ((modMask conf, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask conf, xK_period), sendMessage (IncMasterN (-1)))

    -- Sink floating window
    , ((modMask conf, xK_t), withFocused $ \sid -> sink sid)

    -- Quit wynad
    , ((modMask conf .|. shiftMask, xK_q), io exitWynad)

    -- Restart wynad
    , ((modMask conf, xK_q), restart)
    ]
    ++
    -- mod-[1..9]: Switch to workspace N
    -- mod-shift-[1..9]: Move window to workspace N
    [ ((m .|. modMask conf, k), windows $ f i)
    | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, noModifier), (W.shift, shiftMask)]
    ]
    ++
    -- mod-{w,e,r}: Switch to physical screen 1, 2, 3
    -- mod-shift-{w,e,r}: Move window to screen 1, 2, 3
    [ ((m .|. modMask conf, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, noModifier), (W.shift, shiftMask)]
    ]

-----------------------------------------------------------------------------
-- Default Mouse Bindings
-----------------------------------------------------------------------------

-- | Default mouse bindings.
defaultMouseBindings :: WConfig Layout -> M.Map (Modifiers, Button) (SurfaceId -> W ())
defaultMouseBindings conf = M.fromList
    -- mod-button1: Float and move window
    [ ((modMask conf, button1), \sid -> do
          focus sid
          float sid (RationalRect 0 0 1 1)
          -- TODO: Start mouse drag for moving
      )

    -- mod-button2: Raise window to top
    , ((modMask conf, button2), \sid -> do
          focus sid
          -- TODO: Raise window
      )

    -- mod-button3: Float and resize window
    , ((modMask conf, button3), \sid -> do
          focus sid
          float sid (RationalRect 0 0 1 1)
          -- TODO: Start mouse drag for resizing
      )
    ]

-----------------------------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------------------------

-- | Spawn a command.
spawn :: String -> W ()
spawn cmd = io $ void $ forkProcess $ executeFile "/bin/sh" False ["-c", cmd] Nothing

-- | Exit Wynad.
exitWynad :: IO ()
exitWynad = error "exitWynad: not implemented"

-- | Restart Wynad.
restart :: W ()
restart = io $ error "restart: not implemented"

-----------------------------------------------------------------------------
-- XKB Keysym Constants
-- These match XKB keysym values (same as X11)
-----------------------------------------------------------------------------

xK_Return, xK_Tab, xK_space :: Keysym
xK_Return = Keysym 0xff0d
xK_Tab    = Keysym 0xff09
xK_space  = Keysym 0x0020

xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9 :: Keysym
xK_1 = Keysym 0x0031
xK_2 = Keysym 0x0032
xK_3 = Keysym 0x0033
xK_4 = Keysym 0x0034
xK_5 = Keysym 0x0035
xK_6 = Keysym 0x0036
xK_7 = Keysym 0x0037
xK_8 = Keysym 0x0038
xK_9 = Keysym 0x0039

xK_a, xK_b, xK_c, xK_d, xK_e, xK_f, xK_g, xK_h, xK_i, xK_j :: Keysym
xK_k, xK_l, xK_m, xK_n, xK_o, xK_p, xK_q, xK_r, xK_s, xK_t :: Keysym
xK_u, xK_v, xK_w, xK_x, xK_y, xK_z :: Keysym
xK_a = Keysym 0x0061
xK_b = Keysym 0x0062
xK_c = Keysym 0x0063
xK_d = Keysym 0x0064
xK_e = Keysym 0x0065
xK_f = Keysym 0x0066
xK_g = Keysym 0x0067
xK_h = Keysym 0x0068
xK_i = Keysym 0x0069
xK_j = Keysym 0x006a
xK_k = Keysym 0x006b
xK_l = Keysym 0x006c
xK_m = Keysym 0x006d
xK_n = Keysym 0x006e
xK_o = Keysym 0x006f
xK_p = Keysym 0x0070
xK_q = Keysym 0x0071
xK_r = Keysym 0x0072
xK_s = Keysym 0x0073
xK_t = Keysym 0x0074
xK_u = Keysym 0x0075
xK_v = Keysym 0x0076
xK_w = Keysym 0x0077
xK_x = Keysym 0x0078
xK_y = Keysym 0x0079
xK_z = Keysym 0x007a

xK_comma, xK_period :: Keysym
xK_comma  = Keysym 0x002c
xK_period = Keysym 0x002e
