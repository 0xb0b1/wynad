-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Wayland.Keyboard
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Keyboard handling and XKB integration for Wayland.
--
-----------------------------------------------------------------------------

module Wynad.Wayland.Keyboard
    ( -- * Keyboard Events
      handleKeyPress
    , handleKeyRelease
    , handleModifiers

      -- * Key Binding Dispatch
    , dispatchKeyBinding
    , lookupKeyBinding

      -- * XKB Integration
    , createKeymap
    , getKeysym
    , getModifiers
    ) where

import Wynad.Core
import Wynad.Wayland.Types

import Control.Monad (when)
import Control.Monad.Reader (asks)

import qualified Data.Map as M

-----------------------------------------------------------------------------
-- Keyboard Events
-----------------------------------------------------------------------------

-- | Handle a key press event.
handleKeyPress :: Keycode -> W ()
handleKeyPress keycode = do
    -- Get current modifiers and convert keycode to keysym
    mods <- getCurrentModifiers
    keysym <- keycodeToKeysym keycode

    -- Check for compositor keybindings first
    handled <- dispatchKeyBinding mods keysym

    -- If not handled, forward to focused surface
    when (not handled) $
        forwardKeyToFocused keycode True

-- | Handle a key release event.
handleKeyRelease :: Keycode -> W ()
handleKeyRelease keycode = do
    -- Forward to focused surface
    forwardKeyToFocused keycode False

-- | Handle modifier change events.
handleModifiers :: Modifiers -> W ()
handleModifiers mods = do
    -- TODO: With wlhs bindings:
    -- wlr_seat_keyboard_notify_modifiers seat modifiers
    return ()

-----------------------------------------------------------------------------
-- Key Binding Dispatch
-----------------------------------------------------------------------------

-- | Dispatch a key binding if one matches.
-- Returns True if a binding was found and executed.
dispatchKeyBinding :: Modifiers -> Keysym -> W Bool
dispatchKeyBinding mods keysym = do
    maction <- lookupKeyBinding mods keysym
    case maction of
        Just action -> do
            action
            return True
        Nothing ->
            return False

-- | Look up a key binding.
lookupKeyBinding :: Modifiers -> Keysym -> W (Maybe (W ()))
lookupKeyBinding mods keysym = do
    bindings <- asks keyActions
    return $ M.lookup (mods, keysym) bindings

-----------------------------------------------------------------------------
-- XKB Integration
-----------------------------------------------------------------------------

-- | Create an XKB keymap.
createKeymap :: IO ()
createKeymap = do
    -- TODO: With wlhs/xkbcommon bindings:
    -- xkb_context_new XKB_CONTEXT_NO_FLAGS
    -- xkb_keymap_new_from_names context rules XKB_KEYMAP_COMPILE_NO_FLAGS
    return ()

-- | Convert a keycode to a keysym using XKB.
getKeysym :: Keycode -> W Keysym
getKeysym (Keycode kc) = do
    -- TODO: With wlhs/xkbcommon bindings:
    -- xkb_state_key_get_one_sym xkbState keycode
    return (Keysym kc)  -- Placeholder

-- | Get the current modifier state.
getModifiers :: W Modifiers
getModifiers = do
    -- TODO: With wlhs/xkbcommon bindings:
    -- Get modifier state from xkb_state
    return noModifier  -- Placeholder

-----------------------------------------------------------------------------
-- Internal Helpers
-----------------------------------------------------------------------------

-- | Get current keyboard modifiers.
getCurrentModifiers :: W Modifiers
getCurrentModifiers = getModifiers

-- | Convert a keycode to keysym.
keycodeToKeysym :: Keycode -> W Keysym
keycodeToKeysym = getKeysym

-- | Forward a key event to the focused surface.
forwardKeyToFocused :: Keycode -> Bool -> W ()
forwardKeyToFocused keycode pressed = do
    -- TODO: With wlhs bindings:
    -- wlr_seat_keyboard_notify_key seat time keycode state
    return ()
