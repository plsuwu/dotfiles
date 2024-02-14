import XMonad

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers (logLayoutOnScreen, logTitleOnScreen, shortenL, wrapL)

import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Gaps (Direction2D (D, L, R, U), gaps)
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import XMonad.Layout.ThreeColumns

import Graphics.X11.Xinerama (getScreenInfo)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Hooks.ManageDocks

import Data.List
import qualified Data.List as L

{----  -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------  ----}

myTerminal :: [Char]
myTerminal = "alacritty"
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
myNvim = "alacritty -e tmux new-session nvim"
myBrowser = "/usr/bin/google-chrome-stable"
scriptsPath = "/home/please/.config/xmonad/scripts/"
screenshotFull = "/home/please/.config/xmonad/scripts/screenshot.sh -f"
screenshotSelect = "/home/please/.config/xmonad/scripts/screenshot.sh -s"
myAppLauncher = "rofi -show drun"
restartTray =
    "killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    \--monitor 0 --width 10 --margin 5 --distance 2.5 --iconspacing 7 --expand false"
termAtCwd = "/home/please/.config/xmonad/scripts/term_at_cwd.sh"
gnomePolkit = "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"

-- these spotify commands need a little bit of work (maybe try `amixer` & `dbus`??)
--   -- volume controls
--  , ("M-<Print>", spawn "amixer set Master toggle")
--  , ("M-<Scroll_lock>", spawn "amixer set Master 5%-")
--  , ("M-<Pause>", spawn "amixer set Master 5%+")
--  -- spotify controls
--  , ("M-<F9>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
--  , ("M-<F11>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
--  , ("M-<F12>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
spotifyDaemon = "killall spotify_player; spotify_player -d"
spotifyPausePlay = "playerctl -p spotify_player play-pause"
spotifyNext = "playerctl -p spotify_player next"
spotifyPrev = "playerctl -p spotify_player previous"
spotifyVolU = "spotify_player -d playback volume --offset 5"
spotifyVolD = "spotify_player -d playback volume --offset -- -5"

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    -- applications
    [ ("M-<Return>", spawn "alacritty")
    , ("M-w", spawn "google-chrome-stable")
    , ("M-d", spawn "rofi -show drun")
    , ("M-S-r", spawn $ scriptsPath ++ "recomp.sh")
    , ("M-S-s", spawn $ scriptsPath ++ "screenshot.sh -s")
    , ("M-C-s", spawn $ scriptsPath ++ "screenshot.sh -f")
    , ("M-f", toggleFull)
    , ("M-M1-S-x", spawn "tmux kill-server")
    , ("M-S-t", spawn restartTray)
    , ("M-M1-l", spawn "dm-tool switch-to-greeter")
    , ("M-v", spawn "CM_LAUNCHER=rofi clipmenu")
    , -- spotify
      ("M-<F4>", spawn spotifyVolU)
    , ("M-<F3>", spawn spotifyVolD)
    , ("M-<F5>", spawn spotifyPausePlay)
    , ("M-<F7>", spawn spotifyPrev)
    , ("M-<F8>", spawn spotifyNext)
    , -- general
      ("M-q", kill)
    ]

myLayout =
    gaps [(L, 2), (R, 2), (U, 30), (D, 2)]
        $ spacingRaw
            True
            (Border gap gap gap gap)
            True
            (Border gap gap gap gap)
            True
        $ tiled
            ||| Mirror tiled
            ||| threeCol
            ||| Full
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    gap = 2

toggleFull =
    withFocused
        ( \windowId -> do
            floats <- gets (W.floating . windowset)
            if windowId `M.member` floats
                then withFocused $ windows . W.sink
                else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1)
        )

myManageHook :: ManageHook
myManageHook =
    composeAll
        [ className =? "Eww" --> doIgnore
        , className =? "Eww" --> doFloat
        , className =? "eww" --> doIgnore
        , className =? "eww" --> doFloat
        ]

-- this isnt even used i just don't really want to spend energy on it
ewwPP :: ScreenId -> PP
ewwPP s =
    marshallPP s $
        def
            { ppCurrent = wrap "[" "]"
            }

myStartupHook :: X ()
myStartupHook = do
    spawn "xsetroot -cursor_name left_ptr &"
    spawnOnce "solaar --window=hide"
    spawnOnce "picom --config /home/please/.config/picom/picom.conf -b"
    spawnOnce "feh --bg-tile --no-fehbg /home/please/.config/feh/main.png"
    spawnOnce "/usr/local/bin/eww -c /home/please/.config/eww open-many primary-bar secondary-bar"

-- spawn   spotifyDaemon
-- spawnOn "0" "discord --start-minimized"
-- spawnOn "0" "caprine &"
{-- spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true \
        \--monitor 0 --width 10 --margin 5 --distance 2.5 --iconspacing 7 --expand false" --}

{----  -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------  ----}

main :: IO ()
main =
    xmonad . ewmh . ewmhFullscreen . docks $
        def
            { modMask = mod4Mask
            , layoutHook = myLayout
            , borderWidth = 3
            , focusedBorderColor = "#f4b5d8"
            , normalBorderColor = "#838ba7"
            , terminal = myTerminal
            , workspaces = myWorkspaces
            , startupHook = myStartupHook
            , manageHook = manageSpawn <+> myManageHook
            -- , keys = myKeys
            }
            `additionalKeysP` myAdditionalKeys
