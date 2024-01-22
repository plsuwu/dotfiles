{-- NOTES: ---
-------------------

    https://www.reddit.com/r/xmonad/comments/14576z2/get_workspaces_information_to_integrate_with_eww/

    https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-StatusBar.html
    https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-ManageDocks.html

    https://github.com/lifer0se/dotfiles/blob/master/.config/xmonad/xmonad.hs

-------------------
--}
--------------------------------------------------------------------------------------------------------------------------------------------------------------
---- imports -------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------------------------------------------------------------------------------------
---- variable ------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

myBorderWidth           = 3
myFocusedBorderColor    = "#ca9ee6"
myNormalBorderColor     = "#838ba7"
myWorkspaces            = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myDiscord               = "discord &"
myNvimHome              = "alacritty -e tmux new-session nvim"
myBrowser               = "/usr/bin/google-chrome-stable"
myTerminal              = "/usr/bin/alacritty"
reloadScript            = "/home/pls/.config/xmonad/scripts/recomp.sh"
screenshotFull          = "/home/pls/.config/xmonad/scripts/screenshot.sh -f"
screenshotSelect        = "/home/pls/.config/xmonad/scripts/screenshot.sh -s"
myAppLauncher           = "rofi -show drun"


--myEventHook = ... -- event hook to update on window tag change
--
--myLogHook = dynamicLogWithPP $ def
--    { ppOutput = ... -- output func - e.g write to file/call script
--    , ppCurrent = ... -- defines how to format the current workspace
--    , ppVisible = ... -- defines how to format the visible but non-focused workspaces
--    , ppHidden = ... -- defines how to format hidden workspaces
--    , ppHiddenNoWindows = ... -- defines how to format hidden workspaces with no windows
--    -- etc -> other customization as needed
--    }
--
-- -- add event hook in main:
--main = xmonad $ def $ ...
--    { handleEventHook = myEventHook <+> handleEventHook def
--    , logHook = myLogHook
--    -- ....
--    }

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xrandr --output DP-0 --mode 2560x1440 --pos 0x65 --rate 165.08 --primary --output HDMI-1 --mode 1920x1080 --pos 2560x0 --rotate normal --scale 1.2"
    spawnOnce "~/.fehbg"
    spawnOnce "picom --config /home/pls/.config/picom/picom.conf -b"
    -- spawnOnce "/usr/local/bin/eww -c /home/pls/.config/eww open bar_master"
    spawnOnce "/usr/bin/discord"
    spawn "xsetroot -cursor_name left_ptr &"
    -- spawnOn
--------------------------------------------------------------------------------------------------------------------------------------------------------------
---- layout --------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

myLayout =

  gaps [(L, 4), (R, 4), (U, 35), (D, 4)]
  $ spacingRaw True (Border gap gap gap gap)
                True (Border gap gap gap gap)
                True

  $ tiled
  ||| threeCol
  ||| Full

    where
      threeCol = ThreeColMid nmaster delta ratio
      tiled    = Tall nmaster delta ratio
      nmaster  = 1
      ratio    = 1/2
      delta    = 3/100
      gap      = 2


        {-- fullscreen --}
toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })

--------------------------------------------------------------------------------------------------------------------------------------------------------------
---- window rules --------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

myManageHook = composeAll
    [
      className =? "Eww"                --> doIgnore
    , className =? "Eww - bar"          --> doIgnore
    , className =? "Eww - bar"          --> doFloat
    , className =? "Eww"                --> doFloat
    ] -- <+> manageDocks


myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
    pure $ statusBarProp ("/usr/local/bin/eww -c /home/pls/.config/eww open bar_" ++ show s ++ "_main")
        (pure $ ewwPP (S s))

ewwPP :: ScreenId -> PP
ewwPP s = marshallPP s $ def
    { ppCurrent             = wrap "[" "]"
    , ppVisible             = wrap "<" ">"
    , ppHidden              = wrap "(" ")"
    , ppHiddenNoWindows     = id
    , ppSep                 = ", "
    , ppWsSep               = " : "
    }



--mySB0 :: StatusBarConfig
--mySB0 = statusBarProp "/usr/local/bin/eww -c /home/pls/.config/eww open bar_master" (pure $ myPP 0)

--------------------------------------------------------------------------------------------------------------------------------------------------------------
---- main ----------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = xmonad . ewmh . ewmhFullscreen . dynamicSBs myStatusBarSpawner . docks $ defaults

defaults = def
    {
      {-- re-bind mod to super --}
      modMask = mod4Mask
    , layoutHook = myLayout
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , manageHook = myManageHook
    }
    `additionalKeysP`
    [ ("M-<Return>", spawn myTerminal)
    , ("M-g", spawn myBrowser)
    , ("M-S-r", spawn reloadScript)
    , ("M-d", spawn myAppLauncher)
    , ("M-s", spawn screenshotSelect)
    , ("M-S-s", spawn screenshotFull)
    , ("M-f", toggleFull)
    , ("M-S-e", spawn myNvimHome)
    , ("M-S-k", spawn "tmux kill-server")
    , ("M-k", spawn "tmux kill-session -a")
    ]


