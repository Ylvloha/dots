import XMonad

-- General
import Data.Maybe (isJust)

-- Qualified
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

-- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (ProcessConfig(emacs), spawnPipe)
import XMonad.Util.Ungrab ()

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts

-- Layout
import XMonad.Layout.Spacing

-- System
import System.IO

type Keybind = (String, X ())

(<:>) :: String -> String -> Keybind
(<:>) x y = (x, spawn y)

implSpace i = spacingRaw False (Border i i i i) True (Border i i i i) True

myModMask :: KeyMask
myModMask = mod4Mask

xTerminal :: Keybind
xTerminal = "M-S-<Return>" <:> "alacritty"

xLauncher :: Keybind
xLauncher = "M-p" <:> "rofi -show drun"

xScreenshot :: Keybind
xScreenshot = "<Print>" <:> "maim | xclip -selection clipboard -t image/png"

xLayout =
  avoidStruts $
  smartBorders $
  subLayout [] (smartBorders Simplest) $ implSpace 7 $ layoutHook def
  
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc -d"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc -d"
  xmonad $
    docks $
    ewmhFullscreen $
    ewmh $
    def
      { borderWidth = 0
      , normalBorderColor = "#cccccc"
      , workspaces =
          [ " λ-dev "
          , " λ-sys "
          , " λ-music "
          , " λ-www "
          , " λ-vid "
          , " λ-doc "
          , " λ-gimp "
          , " λ-kvm "
          , " λ-game "
          ]
      , focusedBorderColor = "#cd8b00"
      , manageHook = manageDocks <+> manageHook def
      , layoutHook = avoidStruts $ xLayout -- Add spacing with 10 pixels
      } `additionalKeysP`
    [ xLauncher
    , xTerminal
    , xScreenshot
    ]
