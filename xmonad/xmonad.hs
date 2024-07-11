{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import qualified Data.Map as M
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog)
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.Gaps (Gaps, gaps)
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook ()
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Timer
import XMonad.Util.WindowProperties (getProp32)

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

myTerm :: String
myTerm = "alacritty"

myBrowser :: String
myBrowser = "google-chrome-stable"

myAppLauncher :: String
myAppLauncher = "rofi -show drun"

restartTray :: String
restartTray =
  "killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true --monitor 0 --width 10 \
  \--distance 1 --iconspacing 7 --expand false --tint 0xFFFFFF --transparent true --alpha 0"

gnomePolkit :: String
gnomePolkit = "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"

confDir :: String
confDir = "/home/please/.config"

randomBg :: [Char]
randomBg = "feh --bg-fill --no-xinerama --no-fehbg --randomize " ++ confDir ++ "/feh/wallpaper"

scriptsPath :: [Char]
scriptsPath = confDir ++ "/xmonad/scripts"

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
  [ ("M-<Return>", spawn "alacritty"),
    ("M-w", spawn "google-chrome-stable"),
    ("M-d", spawn "rofi -show drun"),
    ("M-s", spawn $ scriptsPath ++ "/screenshot.sh -s"), -- save image to disk
    ("M-S-s", spawn $ scriptsPath ++ "/screenshot.sh -sc"), -- copy image to clipboard
    ("M-C-s", spawn $ scriptsPath ++ "/screenshot.sh -f"),
    ("M-f", toggleFull),
    ("M-x", spawn "tmux kill-pane"),
    ("M-M1-S-x", spawn "tmux kill-server"),
    ("M-S-t", spawn restartTray),
    ("M-M1-S-b", spawn randomBg),
    ("M-v", spawn "CM_LAUNCHER=rofi clipmenu"),
    ("M-S-c", spawn "gnome-calculator"),
    ("M-q", kill)
  ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    [ ((modm, xK_Tab), windows W.focusDown),
      ((modm, xK_space), sendMessage NextLayout),
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      ((modm, xK_j), windows W.focusDown),
      ((modm, xK_k), windows W.focusUp),
      ((modm, xK_m), windows W.focusMaster),
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      ((modm, xK_h), sendMessage Shrink),
      ((modm, xK_l), sendMessage Expand),
      ((modm, xK_t), withFocused $ windows . W.sink),
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      ((modm .|. shiftMask, xK_q), io exitSuccess),
      ((modm, xK_0), windows $ W.greedyView "0"),
      ((modm .|. shiftMask, xK_0), windows $ W.shift "0")
    ]
      ++ [ ((m .|. modm, k), windows $ f i)
           | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
             (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]
      ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_e, xK_r] [0 ..],
             (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

myLayout :: ModifiedLayout Gaps (ModifiedLayout Spacing (Choose Tall (Choose (Mirror Tall) (Choose ThreeCol Full)))) a
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

toggleFull :: X ()
toggleFull =
  withFocused
    ( \windowId -> do
        floats <- gets (W.floating . windowset)
        if windowId `M.member` floats
          then withFocused $ windows . W.sink
          else withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1)
    )

-- scratchpads :: [NamedScratchpad]
-- scratchpads =
--     [ NS "calculator" "gnome-calculator" (className =? "gnome-calculator") defaultFloating
--     , NS "btop" "btop" (title =? "btop") defaultFloating
--     ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce gnomePolkit
  spawn "systemctl --user import-environment DISPLAY"
  spawnOnce $ "picom --config " ++ confDir ++ "/picom/picom.conf"
  spawnOnce $ "feh --bg-fill --no-xinerama --no-fehbg --randomize " ++ confDir ++ "/feh/wallpaper"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawn "sleep 5 && clipmenud"
  spawn "/home/please/.local/bin/eww -c /home/please/.config/eww open-many primary-bar secondary-bar"
  spawn "solaar --window=hide"
  spawn "wired"
  spawnOn "0" "vesktop"
  spawn
    "sleep 10; killall trayer; trayer --edge top --align right --SetDockType true \
    \--SetPartialStrut true --monitor 0 --width 10 --distance 1 --iconspacing 7 --expand false \
    \--tint 0xFFFFFF --transparent true --alpha 0"

{- WM_CLASS:
    * appName => returns the application name; i.e, the *first* string returned in `WM_CLASS` prop,
    * className => returns the resource class; i.e, the *second* string returned in `WM_CLASS` prop. -}
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ hasNetWMState "_NET_WM_STATE_ABOVE" --> doFloat,
      hasNetWMState "_NET_WM_STATE_STICKY" --> doF copyToAll,
      isDialog --> doF W.shiftMaster,
      stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doCenterFloat,
      stringProperty "WM_NAME" =? "QEMU/KVM - Connection Details" --> doFloat,
      className =? "Eww" --> doIgnore,
      className =? "Solaar" --> doFloat,
      className =? "gnome-calculator" --> doFloat,
      className =? "feh" --> doFloat,
      className =? "vesktop" --> doShift "0"
    ]
  where
    getNetWMState :: Window -> X [Atom]
    getNetWMState w = do
      atom <- getAtom "_NET_WM_STATE"
      maybe [] (map fromIntegral) <$> getProp32 atom w

    hasNetWMState :: String -> Query Bool
    hasNetWMState state = do
      window <- ask
      wmstate <- liftX $ getNetWMState window
      atom <- liftX $ getAtom state
      return $
        elem atom wmstate

main :: IO ()
main =
  xmonad
    . ewmh
    . ewmhFullscreen
    . docks
    $ def
      { modMask = mod4Mask,
        layoutHook = myLayout,
        borderWidth = 3,
        focusedBorderColor = "#ffffff",
        normalBorderColor = "#838ba7",
        terminal = myTerm,
        workspaces = myWorkspaces,
        manageHook = myManageHook <+> manageSpawn <+> manageDocks,
        startupHook = myStartupHook,
        keys = myKeys
      }
      `additionalKeysP` myAdditionalKeys
