import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (hPutStrLn)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
  ( Direction1D(..)
  , WSType(..)
  , moveTo
  , nextScreen
  , prevScreen
  , shiftTo
  )
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (killAll, sinkAll)

import qualified Data.Map as M
import Data.Monoid

import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (EOT(EOT), (??), mkToggle, single)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances
  ( StdTransformers(MIRROR, NBFULL, NOBORDERS)
  )
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts as T
  ( ToggleLayout(Toggle)
  , toggleLayouts
  )
import XMonad.Layout.WindowArranger (WindowArrangerMsg(..), windowArrange)
import XMonad.Layout.WindowNavigation

import XMonad.Hooks.DynamicLog
  ( PP(..)
  , dynamicLogWithPP
  , shorten
  , wrap
  , xmobarColor
  , xmobarPP
  )
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
  ( ToggleStruts(..)
  , avoidStruts
  , docks
  , manageDocks
  )
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isFullscreen)

import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook)

import Colors.GruvboxDark

myTerminal = "alacritty"

myEditor = "fish -c nvim"

myModMask = mod4Mask

myFont = "xft:FiraCode Nerd Font:regular:size=30:antialias=true:hinting=true"

myFocusFollowsMouse = False

myClickJustFocuses = True

myWorkspaces = ["dev", "chat", "web", "mus"]

myBorderWidth = 1

myNormColor = colorBack

myFocusColor = colorFore

myTabTheme =
  def
    { fontName = myFont
    , activeColor = colorFore
    , inactiveColor = color08
    , activeBorderColor = colorFore
    , inactiveBorderColor = colorBack
    , activeTextColor = colorBack
    , inactiveTextColor = color16
    }

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacingNoBorders i =
  spacingRaw True (Border i i i i) True (Border i i i i) True

myKeys =
  [ ("M-<Return>", spawn (myTerminal))
  , ("M-p p", spawn "dm-logout")
  , ("M-p s", spawn "dm-websearch")
  , ("M-S-<Return>", spawn "dmenu_run -l 20 -i -p \"Run: \"")
  , ("M-c", kill)
  , ("M-<F12>", spawn "scrot -s -e 'xclip -selection clipboard -t image/png -i $f'")
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-w", killAll)
  , ("M-n", refresh)
  , ("M-<Tab>", windows W.focusDown)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-m", windows W.focusMaster)
  , ("M-S-m", windows W.swapMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-<Left>", sendMessage Shrink)
  , ("M-<Right>", sendMessage Expand)
  , ("M-<Down>", sendMessage MirrorShrink)
  , ("M-<Up>", sendMessage MirrorExpand)
  , ("M-t", sinkAll)
  , ("M-q q", spawn (myTerminal ++ " -e " ++ myEditor))
  , ("M-S-q", io (exitWith ExitSuccess))
  , ("M-f", spawn (myTerminal ++ " -e ranger"))
  , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
  , ("M-z", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-[", prevScreen)
  , ("M-]", nextScreen)
  ]

myAdditionalKeys =
  [ ((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
  [ ( (modm, button1)
    , (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
  , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
  , ( (modm, button3)
    , (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
  ]

windowCount =
  gets $
  Just .
  show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

floats =
  renamed [Replace "floats"] $ smartBorders $ limitWindows 20 simplestFloat

grid =
  renamed [Replace "grid"] $
  smartBorders $
  windowNavigation $
  addTabs shrinkText myTabTheme $
  subLayout [] (smartBorders Simplest) $
  limitWindows 12 $ mySpacing 2 $ mkToggle (single MIRROR) $ Grid (16 / 10)

threeCol =
  renamed [Replace "threeCol"] $
  smartBorders $
  windowNavigation $
  addTabs shrinkText myTabTheme $
  subLayout [] (smartBorders Simplest) $
  mySpacing 2 $
  limitWindows 7 $ ThreeCol 1 (3 / 100) (1 / 2)

threeRow =
  renamed [Replace "threeRow"] $
  smartBorders $
  windowNavigation $
  addTabs shrinkText myTabTheme $
  subLayout [] (smartBorders Simplest) $
  mySpacing 2 $
  limitWindows 7 $ Mirror $ ThreeCol 1 (3 / 100) (1 / 2)

tall = renamed [Replace "tall"]
  $ smartBorders
  $ windowNavigation
  $ addTabs shrinkText myTabTheme
  $ subLayout [] (smartBorders Simplest)
  $ limitWindows 12
  $ mySpacing 2
  $ ResizableTall 1 (20/100) (1/2) []

myLayoutHook =
  avoidStruts $
  mouseResize $
  windowArrange $
  T.toggleLayouts floats $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall ||| grid ||| threeCol ||| threeRow

myManageHook =
  composeAll
    [ className =? "confirm" --> doFloat
    , className =? "file_progress" --> doFloat
    , className =? "dialog" --> doFloat
    , className =? "download" --> doFloat
    , className =? "error" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "notification" --> doFloat
    , className =? "splash" --> doFloat
    , className =? "toolbar" --> doFloat
    , className =? "blueman-applet" --> doFloat
    , className =? "Blueman-manager" --> doFloat
    , title =? "Oracle VM VirtualBox Manager" --> doFloat
    , title =? "Mozilla Firefox" --> doShift (myWorkspaces !! 2)
    , title =? "Discord" --> doShift (myWorkspaces !! 1)
    , title =? "New Tab - Chromium" --> doShift (myWorkspaces !! 1)
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
    , title =? "Spotify" --> doShift (myWorkspaces !! 3)
    , isFullscreen --> doFullFloat
    ]

myStartupHook = do
  spawn "killall trayer"
  spawnOnce "blueman-applet"
  spawnOnce "/usr/bin/emacs --daemon"
  spawnOnce "picom"
  spawnOnce "nitrogen --restore" 
  spawnOnce "nm-applet"
  spawn ("sleep 2 && trayer --iconspacing 6 --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut false --expand true --monitor 0 --transparent true --alpha 0 " ++ colorTrayer ++ " --height 22")

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/.xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/.xmobarrc"
  xmonad $ ewmh $ docks $ def
        { modMask = myModMask
        , manageHook = myManageHook <+> manageDocks
        , terminal = myTerminal
        , handleEventHook    = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , workspaces = myWorkspaces
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook =
            dynamicLogWithPP $
            xmobarPP
              { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
              , ppCurrent =
                  xmobarColor color10 "" .
                  wrap
                    ("<box type=Bottom width=2 mb=2 color=" ++ color10 ++ ">")
                    "</box>"
              , ppVisible = xmobarColor color10 ""
              , ppHidden =
                  xmobarColor color05 "" .
                  wrap
                    ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">")
                    "</box>"
              , ppHiddenNoWindows = xmobarColor color05 ""
              , ppTitle = xmobarColor color16 "" . shorten 60
              , ppSep = "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
              , ppExtras = [windowCount]
              , ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
              }
        } `additionalKeys`
    myAdditionalKeys `additionalKeysP`
    myKeys
