{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wynad.Core
-- Copyright   :  (c) 2025 Paulo Vicente
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  vcente82@gmail.com
-- Stability   :  experimental
--
-- The 'W' monad, a state monad transformer over 'IO', for the window
-- manager state, and support routines.
--
-----------------------------------------------------------------------------

module Wynad.Core
    ( -- * The W Monad
      W(..)
    , runW
    , io

      -- * State Types
    , WState(..)
    , WConf(..)
    , WConfig(..)

      -- * Window Set Types
    , WindowSet
    , WindowSpace
    , WorkspaceId
    , ScreenId(..)
    , ScreenDetail(..)

      -- * Query Monad
    , Query(..)
    , runQuery
    , ManageHook

      -- * Helper Functions
    , withBackend
    , withWindowSet
    , catchW
    , userCode
    , userCodeDef
    , whenJust
    , whenW
    , ifM
    , trace

      -- * Re-exports
    , Default(..)
    , asks
    , ask
    , gets
    , get
    , put
    , modify
    ) where

import Wynad.StackSet (StackSet, Workspace, Stack)
import Wynad.Wayland.Types
import Wynad.Layout (Layout, LayoutClass)

import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (catch, SomeException(..), throw)
import Data.Default.Class
import Data.Monoid (Endo(..), Ap(..))
import Data.Semigroup
import Data.Typeable
import System.IO (hPrint, hFlush, stderr)

import qualified Data.Map as M
import qualified Data.Set as S

-----------------------------------------------------------------------------
-- Window Set Types
-----------------------------------------------------------------------------

-- | The window set type used by Wynad.
type WindowSet = StackSet WorkspaceId (Layout SurfaceId) SurfaceId ScreenId ScreenDetail

-- | A single workspace.
type WindowSpace = Workspace WorkspaceId (Layout SurfaceId) SurfaceId

-- | Virtual workspace identifier.
type WorkspaceId = String

-- | Physical screen index.
newtype ScreenId = S Int
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Enum, Num, Integral, Real)

-- | Screen detail containing the geometry.
newtype ScreenDetail = SD { screenRect :: Rectangle }
    deriving (Show, Read, Eq)

-----------------------------------------------------------------------------
-- State Types
-----------------------------------------------------------------------------

-- | WState: The mutable window manager state.
data WState = WState
    { windowset       :: !WindowSet           -- ^ Current workspace/window structure
    , mapped          :: !(S.Set SurfaceId)   -- ^ Set of currently mapped surfaces
    , waitingUnmap    :: !(M.Map SurfaceId Int) -- ^ Expected unmap events
    , dragging        :: !(Maybe (Int -> Int -> W (), W ()))
                                              -- ^ Current drag operation
    , extensibleState :: !(M.Map String (Either String StateExtension))
                                              -- ^ Custom state storage
    }

-- | WConf: The read-only window manager configuration.
data WConf = WConf
    { config        :: !(WConfig Layout)      -- ^ User configuration
    , normalBorder  :: !Color                 -- ^ Unfocused border color
    , focusedBorder :: !Color                 -- ^ Focused border color
    , keyActions    :: !(M.Map (Modifiers, Keysym) (W ()))
                                              -- ^ Key binding map
    , buttonActions :: !(M.Map (Modifiers, Button) (SurfaceId -> W ()))
                                              -- ^ Mouse binding map
    , currentEvent  :: !(Maybe SomeEvent)     -- ^ Event being processed
    }

-- | Placeholder for Wayland events.
-- TODO: Replace with actual wlroots event types when wlhs is integrated.
data SomeEvent = SomeEvent
    deriving (Show, Eq)

-----------------------------------------------------------------------------
-- Configuration Types
-----------------------------------------------------------------------------

-- | User-facing configuration type.
data WConfig l = WConfig
    { normalBorderColor  :: !Color            -- ^ Unfocused border color
    , focusedBorderColor :: !Color            -- ^ Focused border color
    , terminal           :: !String           -- ^ Preferred terminal
    , layoutHook         :: !(l SurfaceId)    -- ^ Available layouts
    , manageHook         :: !ManageHook       -- ^ New window handler
    , workspaces         :: ![WorkspaceId]    -- ^ Workspace names
    , modMask            :: !Modifiers        -- ^ Modifier key
    , keys               :: !(WConfig Layout -> M.Map (Modifiers, Keysym) (W ()))
                                              -- ^ Key bindings
    , mouseBindings      :: !(WConfig Layout -> M.Map (Modifiers, Button) (SurfaceId -> W ()))
                                              -- ^ Mouse bindings
    , borderWidth        :: !Int              -- ^ Border width in pixels
    , logHook            :: !(W ())           -- ^ Called after window set changes
    , startupHook        :: !(W ())           -- ^ Called at startup
    , focusFollowsMouse  :: !Bool             -- ^ Focus follows pointer?
    , clickJustFocuses   :: !Bool             -- ^ Click just focuses (not passed through)?
    }

instance Default (WConfig l) where
    def = WConfig
        { normalBorderColor  = rgbColor 0x44 0x44 0x44
        , focusedBorderColor = rgbColor 0x00 0xff 0x00
        , terminal           = "foot"
        , layoutHook         = error "layoutHook not set"
        , manageHook         = mempty
        , workspaces         = map show [1..9 :: Int]
        , modMask            = mod4Mask
        , keys               = const mempty
        , mouseBindings      = const mempty
        , borderWidth        = 2
        , logHook            = return ()
        , startupHook        = return ()
        , focusFollowsMouse  = True
        , clickJustFocuses   = True
        }

-----------------------------------------------------------------------------
-- Extensible State
-----------------------------------------------------------------------------

-- | Every module must make its state an instance of this class.
class Typeable a => ExtensionClass a where
    initialValue :: a
    extensionType :: a -> StateExtension
    extensionType = StateExtension

-- | Existential type to store a state extension.
data StateExtension
    = forall a. ExtensionClass a => StateExtension a
    | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a

-----------------------------------------------------------------------------
-- The W Monad
-----------------------------------------------------------------------------

-- | The W monad: ReaderT and StateT transformers over IO.
-- Encapsulates the window manager configuration and state.
newtype W a = W (ReaderT WConf (StateT WState IO) a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState WState, MonadReader WConf, MonadFail)
    deriving (Semigroup, Monoid) via Ap W a

instance Default a => Default (W a) where
    def = return def

-- | Run the W monad with the given configuration and initial state.
runW :: WConf -> WState -> W a -> IO (a, WState)
runW c st (W a) = runStateT (runReaderT a c) st

-- | Lift an IO action into the W monad.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Run in the W monad, catching exceptions.
catchW :: W a -> W a -> W a
catchW job errcase = do
    st <- get
    c <- ask
    (a, s') <- io $ runW c st job `catch` \e -> case e of
        SomeException _ -> do
            hPrint stderr e
            runW c st errcase
    put s'
    return a

-- | Execute an action, catching all exceptions.
userCode :: W a -> W (Maybe a)
userCode a = catchW (Just <$> a) (return Nothing)

-- | Like userCode but with a default value.
userCodeDef :: a -> W a -> W a
userCodeDef defValue a = maybe defValue id <$> userCode a

-----------------------------------------------------------------------------
-- Query Monad (ManageHook)
-----------------------------------------------------------------------------

-- | A ManageHook is a Query that returns a WindowSet transformation.
type ManageHook = Query (Endo WindowSet)

-- | The Query monad for querying surface properties.
newtype Query a = Query (ReaderT SurfaceId W a)
    deriving newtype (Functor, Applicative, Monad, MonadReader SurfaceId, MonadIO)
    deriving (Semigroup, Monoid) via Ap Query a

instance Default a => Default (Query a) where
    def = return def

-- | Run a Query on a surface.
runQuery :: Query a -> SurfaceId -> W a
runQuery (Query m) = runReaderT m

-----------------------------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------------------------

-- | Run an action with the current backend.
-- TODO: Add backend to WConf when wlhs is integrated.
withBackend :: (a -> W b) -> W b
withBackend _ = error "withBackend: backend not yet implemented"

-- | Run an action with the current window set.
withWindowSet :: (WindowSet -> W a) -> W a
withWindowSet f = gets windowset >>= f

-- | If-then-else lifted to a Monad.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f

-- | Conditionally run an action using a Maybe.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action using a W Bool.
whenW :: W Bool -> W () -> W ()
whenW a f = a >>= \b -> when b f

-- | Log a message to stderr.
trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr
    where hPutStrLn h s = hPrint h s >> hFlush h
