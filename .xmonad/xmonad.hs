-- last update: January 2018 by ngyj (https://github.com/ngyj)
-- written by Shotaro Fujimoto (https://github.com/ssh0)
-- modified and repaired by Ogis (https://github.com/Minda1975)

import qualified Data.Map as M
import           System.IO (hPutStrLn)

import           XMonad
import qualified XMonad.StackSet as W -- myManageHookShift

import           XMonad.Actions.CopyWindow (kill1)
import           XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev, toggleWS, nextScreen)
import qualified XMonad.Actions.FlexibleResize as Flex (mouseResizeWindow)
import           XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
import           XMonad.Actions.UpdatePointer (updatePointer)
import           XMonad.Actions.DwmPromote (dwmpromote)

import           XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, xmobarPP, xmobarColor)
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (manageDocks, docksEventHook, avoidStruts)
import           XMonad.Hooks.ManageHelpers (Side(..), doCenterFloat, doSideFloat,
                                             doFullFloat, isDialog, isFullscreen)

import           XMonad.Layout.LayoutScreens (layoutScreens)
import           XMonad.Layout.NoBorders (noBorders, smartBorders)
import           XMonad.Layout.Renamed (Rename(..), renamed)
import           XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import           XMonad.Layout.Spacing (spacing)
import           XMonad.Layout.ThreeColumns (ThreeCol(..))
import           XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import           XMonad.Layout.TwoPane (TwoPane(..))

import           XMonad.Prompt (XPConfig(..), XPPosition(..))
import           XMonad.Prompt.Window (windowPromptGoto, windowPromptBring)
import           XMonad.Util.EZConfig (removeKeysP, additionalKeysP)
import           XMonad.Util.Run (spawnPipe)      -- spawnPipe, hPutStrLn

import qualified Graphics.X11.ExtraTypes.XF86 as XF86

-- config stuff
myWorkspaces = map show [1..9]
modm = mod4Mask

-- Arc Color Setting
colorfg         = "#ece8cf"
colorDarkbg     = "#2d2d2d"
colorNormalbg   = "#2d2d2d"

colorBlue       = "#a2c3cc"
colorGreen      = "#dbffb3"
colorRed        = "#ff8d80"
colorWhite      = "#ede7b4"

-- Border width
borderwidth = 0

-- Border color
normalBC  = "#333333"
focusedBC = "#cca8c9"

-- Float window control width
moveWD = borderwidth
resizeWD = 2*borderwidth

-- gapwidth
gapwidth  = 9
gwU = 1
gwD = 0
gwL = 42
gwR = 42

main :: IO ()
main = do
    wsbar <- spawnPipe myWsBar
    xmonad $ ewmh def
      { borderWidth        = borderwidth
      , terminal           = "urxvt"
      , focusFollowsMouse  = True
      , normalBorderColor  = normalBC
      , focusedBorderColor = focusedBC
      , manageHook         = myManageHookFloat <+> manageDocks
      , layoutHook         = avoidStruts
                             . toggleLayouts (noBorders Full)
                             $ myLayout
      , logHook = myLogHook wsbar >> updatePointer (0.5,0.5) (0,0)
      , handleEventHook    = fullscreenEventHook <+> docksEventHook
      , workspaces         = myWorkspaces
      , modMask            = modm
      , mouseBindings      = newMouse
      }
      `removeKeysP`
      [ "M-S-p" -- Unused gmrun binding
      , "M-S-c" -- Unused close window binding
      , "M-S-<Return>"
      ]
      `additionalKeysP` -- window management
      -- Shrink / Expand the focused window
      [ ("M-,"    , sendMessage Shrink)
      , ("M-."    , sendMessage Expand)
      , ("M-z"    , sendMessage MirrorShrink)
      , ("M-a"    , sendMessage MirrorExpand)
      -- Close the focused window
      , ("M-S-c"    , kill1)
      -- Toggle layout (Fullscreen mode)
      , ("M-f"    , sendMessage ToggleLayout)
      , ("M-S-f"  , withFocused (keysMoveWindow (-borderwidth,-borderwidth)))
      -- toggle layout (simplest float)
      , ("M-u"    , sendMessage (Toggle "Simplest"))
      -- Move the focused window
      , ("M-C-<R>", withFocused (keysMoveWindow (moveWD, 0)))
      , ("M-C-<L>", withFocused (keysMoveWindow (-moveWD, 0)))
      , ("M-C-<U>", withFocused (keysMoveWindow (0, -moveWD)))
      , ("M-C-<D>", withFocused (keysMoveWindow (0, moveWD)))
      -- Resize the focused window
      , ("M-s"    , withFocused (keysResizeWindow (-resizeWD, resizeWD) (0.5, 0.5)))
      , ("M-i"    , withFocused (keysResizeWindow (resizeWD, resizeWD) (0.5, 0.5)))
      -- Increase / Decrese the number of master pane
      , ("M-S-;"  , sendMessage $ IncMasterN 1)
      , ("M--"    , sendMessage $ IncMasterN (-1))
      -- Workspace switching/shifting
      , ("M-<R>"  , nextWS )
      , ("M-<L>"  , prevWS )
      , ("M-l"    , nextWS )
      , ("M-h"    , prevWS )
      , ("M-S-<R>", shiftToNext)
      , ("M-S-<L>", shiftToPrev)
      , ("M-S-l"  , shiftToNext)
      , ("M-S-h"  , shiftToPrev)
      -- focus
      , ("M-<D>"  , windows W.focusDown)
      , ("M-<U>"  , windows W.focusUp)
      , ("M-j"    , windows W.focusDown)
      , ("M-k"    , windows W.focusUp)
      , ("M-S-j"  , windows W.swapDown)
      , ("M-S-k"  , windows W.swapUp)
      , ("M-S-<D>"  , windows W.swapDown)
      , ("M-S-<U>"  , windows W.swapUp)
      -- Shift the focused window to the master window
      , ("M-S-m"  , windows W.shiftMaster)
      -- Move the focus to next screen (multi screen)
      , ("M-<Tab>", nextScreen)
      -- Now we have more than one screen by dividing a single screen
      , ("M-C-<Space>", layoutScreens 2 (TwoPane 0.5 0.5))
      , ("M-C-S-<Space>", rescreen)
      ]
      `additionalKeysP` -- custom commands
      [ ("M-S-<Return>", spawn "urxvt")
      , ("M-S-f", spawn "krusader")
      , ("M-S-n", spawn "firefox-devedition")
      , ("M-S-m", spawn "Discord")
      , ("M-S-b", spawn "emacs")
      , ("M-d", spawn "dmenu_run")
      , ("M-S-l", spawn "dm-tool switch-to-greeter")
      , ("M-<Return>", dwmpromote) -- Zoomswap dwm like
      , ("M-<Tab>", toggleWS)
      -- Play / Pause media keys : "<XF86AudioPlay>"  "<XF86HomePage>"
      , ("<XF86AudioRaiseVolume>", spawn "volume_wrapper +")
      , ("<XF86AudioLowerVolume>", spawn "volume_wrapper -")
      , ("<XF86AudioMute>"       , spawn "volume_wrapper m")
      , ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1")
      , ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1")
      ]
      `additionalKeysP`
      [(otherMasks ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
        | (key, scr) <- zip "wer" [0,1,2]
        , (otherMasks, action) <- [ ("", W.view)
                                  , ("S-", W.shift)]
      ]
    where
      myMouse x = [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
      newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))

myLayout =  tiled ||| mtiled ||| full ||| threecol
  where
    nmaster  = 1     -- Default number of windows in master pane
    delta    = 2/100 -- Percentage of the screen to increment when resizing
    ratio    = 5/8   -- Defaul proportion of the screen taken up by main pane
    rt       = spacing 5 $ ResizableTall nmaster delta ratio []
    tiled    = renamed [Replace "V"] $ smartBorders rt
    mtiled   = renamed [Replace "H"] $ smartBorders $ Mirror rt
    full     = renamed [Replace "F"] $ noBorders Full
    threecol = renamed [Replace "3c"] $ ThreeColMid 1 (3/100) (1/2)

myManageHookFloat = composeAll
    [ className =? "Gimp"             --> doFloat
    , className =? "mpv"              --> doCenterFloat
    , className =? "feh"              --> doCenterFloat
    , className =? "Thunar"           --> doCenterFloat
    , className =? "Websearch"        --> doCenterFloat
    , className =? "Pavucontrol"      --> doSideFloat NE
    , title     =? "urxvt_float"      --> doSideFloat SC
    , isFullscreen                    --> doFullFloat
    , isDialog                        --> doCenterFloat
    ]

-- xmobar
myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }

myWsBar = "xmobar $HOME/.xmonad/xmobarrc"

wsPP = xmobarPP
    { ppOrder           = \(ws:l:t:_)  -> [ws,l,t]
    , ppCurrent         = xmobarColor colorfg   colorDarkbg . \s -> "="++s++"="
    , ppUrgent          = xmobarColor colorRed  colorDarkbg . \s -> "<"++s++">"
    , ppVisible         = xmobarColor colorfg   colorDarkbg . \s -> "~"++s++"~"
    , ppHidden          = xmobarColor colorfg   colorDarkbg . \s -> "-"++s++"-"
    , ppHiddenNoWindows = xmobarColor colorfg   colorDarkbg . \s -> "_"++s++"_"
    , ppTitle           = xmobarColor colorfg   colorDarkbg
    , ppOutput          = putStrLn
    , ppWsSep           = " "
    , ppSep             = "  "
    }

myXPConfig = def
    { font              = "tewi"
    , fgColor           = colorfg
    , bgColor           = colorDarkbg
    , borderColor       = colorDarkbg
    , height            = 35
    , promptBorderWidth = 0
    , autoComplete      = Just 100000
    , bgHLight          = colorDarkbg
    , fgHLight          = colorRed
    , position          = Bottom
    }
