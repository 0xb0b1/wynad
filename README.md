# Wynad

An XMonad-inspired Wayland compositor built with Haskell and wlroots.

## Design Philosophy

- **Minimal core with maximum extensibility** - Small, well-defined core with hooks for customization
- **Pure functional window management** - Zipper-based StackSet for workspace/window management
- **Configurable via Haskell** - User configuration compiled into the binary
- **Tiling-first with floating support** - Automatic tiling layouts with optional floating windows
- **Multi-monitor support** - Full support for multiple outputs via wlr_output_layout

## Building

### Prerequisites

System dependencies:
- wlroots 0.17+
- wayland-server
- libxkbcommon
- libinput

Haskell toolchain:
- GHC 9.2+
- cabal-install 3.6+

### Build

```bash
cabal update
cabal build
```

### Run

```bash
# Nested in existing Wayland compositor (for development)
WLR_BACKENDS=wayland cabal run wynad

# On bare metal (from TTY)
cabal run wynad

# Headless (for testing)
WLR_BACKENDS=headless cabal run wynad
```

## Configuration

Create `~/.config/wynad/wynad.hs`:

```haskell
import Wynad
import Wynad.Config
import Wynad.Layout

main :: IO ()
main = wynad def
    { terminal = "foot"
    , modMask = mod4Mask  -- Super key
    , borderWidth = 2
    , normalBorderColor = Color 0x444444
    , focusedBorderColor = Color 0x00ff00
    , layoutHook = Tall 1 (3/100) (1/2) ||| Full
    }
```

## Architecture

```
src/Wynad/
├── Core.hs          -- W monad, WConf, WState
├── StackSet.hs      -- Window/workspace management (from XMonad)
├── Layout.hs        -- LayoutClass, Tall, Full, Mirror
├── Operations.hs    -- manage, unmanage, windows, refresh
├── ManageHook.hs    -- Window rules (Query monad)
├── Config.hs        -- Default configuration
└── Wayland/
    ├── Backend.hs   -- wlroots initialization
    ├── Output.hs    -- Multi-monitor support
    ├── Surface.hs   -- Surface lifecycle
    ├── Input.hs     -- Keyboard/pointer handling
    └── Protocols/
        ├── XdgShell.hs   -- XDG shell protocol
        └── LayerShell.hs -- Layer shell (panels, bars)
```

## License

BSD-3-Clause
