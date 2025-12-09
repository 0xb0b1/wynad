-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Wynad executable entry point.
--
-----------------------------------------------------------------------------

module Main (main) where

import Wynad
import Wynad.Config (defaultConfig)

-- | Main entry point.
-- This uses the default configuration.
-- Users should create their own configuration in ~/.config/wynad/wynad.hs
main :: IO ()
main = wynad defaultConfig
