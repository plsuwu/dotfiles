import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers (logLayoutOnScreen, logTitleOnScreen, shortenL, wrapL)

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Gaps (gaps, Direction2D(D, L, R, U))
import XMonad.Layout.Spacing (spacingRaw, Border(Border))
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerWorkspace

import Graphics.X11.Xinerama (getScreenInfo)

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen (onlyOnScreen)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks

import Data.List
import qualified Data.List as L

{----  -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------  ----}

myBorderWidth           = 3
myFocusedBorderColor    = "#ca9ee6"
myNormalBorderColor     = "#838ba7"
myWorkspaces            = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
myNvim                  = "alacritty -e tmux new-session nvim"
myBrowser               = "/usr/bin/google-chrome-stable"
myTerminal              = "/usr/bin/alacritty"
reloadScript            = "/home/pls/.config/xmonad/scripts/recomp.sh"
screenshotFull          = "/home/pls/.config/xmonad/scripts/screenshot.sh -f"
screenshotSelect        = "/home/pls/.config/xmonad/scripts/screenshot.sh -s"
myAppLauncher           = "rofi -show drun"
mySysTray               = "killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true \
                            \--monitor 1 --width 20 --margin 5 --distance 2.5 --iconspacing 7 --expand false"


myLayout =

  gaps [(L, 2), (R, 2), (U, 35), (D, 2)]
  $ spacingRaw True (Border gap gap gap gap)
                True (Border gap gap gap gap)
                True

  $ tiled
  ||| Mirror tiled
  ||| threeCol
  ||| Full

    where
      threeCol = ThreeColMid nmaster delta ratio
      tiled    = Tall nmaster delta ratio
      nmaster  = 1
      ratio    = 1/2
      delta    = 3/100
      gap      = 2

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })

{----  -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------  ----}

myManageHook :: ManageHook
myManageHook = composeAll
    [
      className =? "Eww"                --> doIgnore
    , className =? "Eww"                --> doFloat
    , className =? "eww"                --> doIgnore
    , className =? "eww"                --> doFloat
    ]


ewwPP :: ScreenId -> PP
ewwPP s = marshallPP s $ def
    { ppCurrent             = wrap "[" "]"
    , ppVisible             = wrap "<" ">"
    , ppHidden              = wrap "(" ")"
    , ppHiddenNoWindows     = id
    , ppSep                 = ", "
    , ppWsSep               = " : "
    }

-- myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
-- myStatusBarSpawner (S s) = do
--     -- pure $ statusBarProp ("/usr/local/bin/eww -c /home/pls/.config/eww open-many bar_" ++ show s ++ "_main")
--     pure $ statusBarPropTo ("_XMONAD_LOG_bar_" ++ show s)
--         ("/usr/local/bin/eww -c /home/pls/.config/eww open-many bar_0_main bar_1_main")
--         (pure $ ewwPP (S s))

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "/usr/local/bin/eww -c /home/pls/.config/eww open-many bar_0_main bar_1_main"
    spawnOnce "xrandr --output DP-0 --mode 2560x1440 --pos 0x0 --rate 165.08 --primary --output HDMI-1 --mode 1920x1080 --pos 2560x0 --rotate normal --scale 1.2"
    spawnOnce "feh --bg-fill --no-fehbg /home/pls/.config/feh/mikumain.png"
    spawnOnce "picom --config /home/pls/.config/picom/picom.conf -b"
    spawnOn "0" "discord --start-minimized &"
    spawnOn "0" "caprine &"
    spawnOnce mySysTray

{----  -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------  ----}

main :: IO ()
main = xmonad . ewmh . ewmhFullscreen . dynamicSBs myStatusBarSpawner . docks $ def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , manageHook = manageSpawn <+> myManageHook
    }
    `additionalKeysP`
    [ ("M-<Return>", spawn myTerminal)
    , ("M-g", spawn myBrowser)
    , ("M-S-r", spawn reloadScript)
    , ("M-d", spawn myAppLauncher)
    , ("M-s", spawn screenshotSelect)
    , ("M-S-s", spawn screenshotFull)
    , ("M-f", toggleFull)
    , ("M-S-e", spawn myNvim)
    , ("M-M1-c", spawn "tmux kill-server")
    , ("M-S-t", spawn mySysTray)
    ]

