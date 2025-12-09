{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.ManageHook
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- Window rules using the Query monad.
-- ManageHooks allow you to specify rules for newly created windows.
--
-----------------------------------------------------------------------------

module Wynad.ManageHook
    ( -- * ManageHook Type
      ManageHook
    , Query

      -- * Query Functions
    , title
    , appId
    , className
    , appName

      -- * ManageHook Combinators
    , composeAll
    , composeOne
    , (<&&>)
    , (<||>)
    , (-->)
    , (-?>)

      -- * ManageHook Actions
    , doFloat
    , doShift
    , doIgnore
    , doSink

      -- * Helpers
    , idHook
    , liftW
    ) where

import Wynad.Core
import Wynad.Wayland.Types
import qualified Wynad.StackSet as W

import Control.Monad (liftM2)
import Control.Monad.Reader (ask)
import Data.Monoid (Endo(..), All(..), (<>))

-----------------------------------------------------------------------------
-- Query Functions
-----------------------------------------------------------------------------

-- | Query the title of a surface.
title :: Query String
title = do
    sid <- ask
    -- TODO: Query actual XDG toplevel title from Wayland backend
    return ""

-- | Query the app_id of a surface (Wayland equivalent of WM_CLASS).
appId :: Query String
appId = do
    sid <- ask
    -- TODO: Query actual XDG toplevel app_id from Wayland backend
    return ""

-- | Query the class name of a surface.
-- In Wayland, this returns the app_id.
className :: Query String
className = appId

-- | Query the application name of a surface.
-- In Wayland, this returns the app_id.
appName :: Query String
appName = appId

-----------------------------------------------------------------------------
-- ManageHook Combinators
-----------------------------------------------------------------------------

-- | Compose a list of manage hooks.
composeAll :: [ManageHook] -> ManageHook
composeAll = mconcat

-- | Compose manage hooks, using the first one that returns a non-identity action.
composeOne :: [Query (Maybe (Endo WindowSet))] -> ManageHook
composeOne = foldr go (return mempty)
    where
        go q rest = do
            result <- q
            case result of
                Just action -> return action
                Nothing     -> rest

-- | Combine two queries with AND.
(<&&>) :: Query Bool -> Query Bool -> Query Bool
(<&&>) = liftM2 (&&)
infixr 3 <&&>

-- | Combine two queries with OR.
(<||>) :: Query Bool -> Query Bool -> Query Bool
(<||>) = liftM2 (||)
infixr 2 <||>

-- | If the query returns True, apply the ManageHook.
(-->) :: Query Bool -> ManageHook -> ManageHook
p --> f = p >>= \b -> if b then f else return mempty
infixr 0 -->

-- | If the query returns True, apply the ManageHook (for use with composeOne).
(-?>) :: Query Bool -> ManageHook -> Query (Maybe (Endo WindowSet))
p -?> f = do
    b <- p
    if b
        then Just <$> f
        else return Nothing
infixr 0 -?>

-----------------------------------------------------------------------------
-- ManageHook Actions
-----------------------------------------------------------------------------

-- | Float the new window.
doFloat :: ManageHook
doFloat = do
    sid <- ask
    return $ Endo $ W.float sid (RationalRect 0.25 0.25 0.5 0.5)

-- | Move the new window to a specific workspace.
doShift :: WorkspaceId -> ManageHook
doShift ws = do
    sid <- ask
    return $ Endo $ W.shiftWin ws sid

-- | Ignore the new window (don't manage it).
-- Note: This removes the window from the StackSet, but it will still be visible
-- if it's a layer-shell surface or has special handling.
doIgnore :: ManageHook
doIgnore = do
    sid <- ask
    return $ Endo $ W.delete sid

-- | Sink a floating window back to tiling.
doSink :: ManageHook
doSink = do
    sid <- ask
    return $ Endo $ W.sink sid

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------

-- | The identity ManageHook (does nothing).
idHook :: ManageHook
idHook = return mempty

-- | Lift a W action into the Query monad.
liftW :: W a -> Query a
liftW = Query . lift
    where lift = error "liftW: not implemented"
