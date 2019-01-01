-- last update: december 2018 by ngyj
-- written by Shotaro Fujimoto (https://github.com/ssh0)
-- modified and repaired by Ogis (https://github.com/Minda1975)

import qualified Data.Map as M
import Control.Monad (liftM2)          -- myManageHookShift
import Data.Monoid
import System.IO                       -- for xmobar

import XMonad
import qualified XMonad.StackSet as W  -- myManageHookShift

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex -- flexible resize
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.DwmPromote (dwmpromote)

import XMonad.Hooks.DynamicLog         -- for xmobar
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks        -- avoid xmobar area
import XMonad.Hooks.ManageHelpers

import XMonad.Layout
import XMonad.Layout.DragPane          -- see only two window
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders         -- In Full mode, border is no use
import XMonad.Layout.PerWorkspace      -- Configure layouts on a per-workspace
import XMonad.Layout.ResizableTile     -- Resizable Horizontal border
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing           -- this makes smart space around windows
import XMonad.Layout.ToggleLayouts     -- Full window at any time
import XMonad.Layout.TwoPane
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid

import XMonad.Prompt
import XMonad.Prompt.Window (windowPromptGoto, windowPromptBring)
import XMonad.Util.EZConfig (removeKeysP, additionalKeysP)
import XMonad.Util.Run (spawnPipe)      -- spawnPipe, hPutStrLn

import qualified Graphics.X11.ExtraTypes.XF86 as XF86

-- config stuff
myWorkspaces = map show [1..9]
modm = mod4Mask

-- Arc Color Setting
colorfg         = "#a0a0a0"
colorDarkbg     = "#101010"
colorNormalbg   = "#202020"

colorBlue       = "#D8DEE9"
colorGreen      = "#5E81AC"
colorRed        = "#BF616A"
colorWhite      = "#efefef"

-- Border width
borderwidth = 0

-- Border color
mynormalBorderColor  = "#333333"
--myfocusedBorderColor = "#585858"
myfocusedBorderColor = "#cca8c9"

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
       , normalBorderColor  = mynormalBorderColor
       , focusedBorderColor = myfocusedBorderColor
       , manageHook         = myManageHookShift <+>
                              myManageHookFloat <+>
                              manageDocks
       , layoutHook         = avoidStruts $ (toggleLayouts (noBorders Full)
                                            -- $ onWorkspace "3" simplestFloat
                                            $ myLayout)
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
       [
       -- Shrink / Expand the focused window
         ("M-,"    , sendMessage Shrink)
       , ("M-."    , sendMessage Expand)
       , ("M-z"    , sendMessage MirrorShrink)
       , ("M-a"    , sendMessage MirrorExpand)
       -- Close the focused window
       , ("M-S-c"    , kill1)
       -- Toggle layout (Fullscreen mode)
       , ("M-f"    , sendMessage ToggleLayout)
       --, ("M-S-f"  , withFocused (keysMoveWindow (-borderwidth,-borderwidth)))
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
       -- Go to the next / previous workspace
       , ("M-<R>"  , nextWS )
       , ("M-<L>"  , prevWS )
       , ("M-l"    , nextWS )
       , ("M-h"    , prevWS )
       -- Shift the focused window to the next / previous workspace
       , ("M-S-<R>", shiftToNext)
       , ("M-S-<L>", shiftToPrev)
       , ("M-S-l"  , shiftToNext)
       , ("M-S-h"  , shiftToPrev)
       -- CopyWindow
       , ("M-v"    , windows copyToAll)
       , ("M-S-v"  , killAllOtherCopies)
       -- Move the focus down / up
       , ("M-<D>"  , windows W.focusDown)
       , ("M-<U>"  , windows W.focusUp)
       , ("M-j"    , windows W.focusDown)
       , ("M-k"    , windows W.focusUp)
       -- Swap the focused window down / up
       , ("M-S-j"  , windows W.swapDown)
       , ("M-S-k"  , windows W.swapUp)
       , ("M-S-<D>"  , windows W.swapDown)
       , ("M-S-<U>"  , windows W.swapUp)
       -- Shift the focused window to the master window
       , ("M-S-m"  , windows W.shiftMaster)
       -- Search a window and focus into the window
       , ("M-g"    , windowPromptGoto myXPConfig)
       -- Search a window and bring to the current workspace
       , ("M-b"    , windowPromptBring myXPConfig)
       -- Move the focus to next screen (multi screen)
       , ("M-<Tab>", nextScreen)
       -- Now we have more than one screen by dividing a single screen
       , ("M-C-<Space>", layoutScreens 2 (TwoPane 0.5 0.5))
       , ("M-C-S-<Space>", rescreen)
       ]
       `additionalKeysP` -- custom commands
       [ ("M-<Return>", dwmpromote) -- Zoomswap dwm like
       , ("M-w", spawn "spotify")
       , ("M-S-<Return>", spawn "urxvt")
       , ("M-S-f", spawn "pcmanfm")
       , ("M-S-n", spawn "firefox")
       , ("M-p", spawn "dmenu_run")
       , ("M-S-l", spawn "slock")
       , ("M-<Tab>", toggleWS)
       -- Play / Pause media keys
       --, ("<XF86AudioPlay>"  , spawn "ncmpcpp toggle")
       --, ("<XF86HomePage>"   , spawn "ncmpcpp toggle")
       --, ("S-<F6>"           , spawn "ncmpcpp toggle")
       --, ("S-<XF86AudioPlay>", spawn "streamradio pause")
       --, ("S-<XF86HomePage>" , spawn "streamradio pause")
       -- Volume setting media keys
       --, ("<XF86AudioRaiseVolume>", spawn "sound_volume_change_wrapper.sh +")
       --, ("<XF86AudioLowerVolume>", spawn "sound_volume_change_wrapper.sh -")
       --, ("<XF86AudioMute>"       , spawn "sound_volume_change_wrapper.sh m")
        -- Brightness Keys
       --, ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1")
       --, ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1")
       -- Take a screenshot (whole window)
       , ("<Print>", spawn "scrot")
       ]
       `additionalKeysP`
       [(otherMasks ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr) <- zip "wer" [2,0,1]
         , (otherMasks, action) <- [ ("", W.view)
                                   , ("S-", W.shift)]
       ]
       --  [ (otherMasks ++ "M-" ++ key, action tag)
       --  | (tag, key) <- zip myWorkspaces (map show [1..9])
       --  , (otherMasks, action) <- [("", windows . W.view) -- replace W.greedyView
       --                               , ("S-", windows . W.shift)]
       --  ]

myLayout =  tiled ||| mtiled ||| full ||| threecol ||| grid
  where
    nmaster  = 1     -- Default number of windows in master pane
    delta    = 2/100 -- Percentage of the screen to increment when resizing
    ratio    = 5/8   -- Defaul proportion of the screen taken up by main pane
    rt       = spacing 5 $ ResizableTall nmaster delta ratio []
    tiled    = renamed [Replace "T"] $ smartBorders rt
    mtiled   = renamed [Replace "Bs"] $ smartBorders $ Mirror rt
    full     = renamed [Replace "M"] $ noBorders Full
    threecol = renamed [Replace "3c"] $ ThreeColMid 1 (3/100) (1/2)
    grid     = renamed [Replace "G"] $ GridRatio (3/3)

myManageHookShift = composeAll
            -- if you want to know className, type "$ xprop|grep CLASS" on shell
            [ className =? "Gimp"       --> mydoShift "3"
            ]
             where mydoShift = doF . liftM2 (.) W.greedyView W.shift

myManageHookFloat = composeAll
    [ className =? "Gimp"             --> doFloat
    , className =? "SMPlayer"               --> doFloat
    , className =? "mpv"              --> doCenterFloat
    , className =? "feh"              --> doCenterFloat
    , className =? "Audacious"        --> doCenterFloat
    --, className =? "Thunar"           --> doCenterFloat
    , className =? "Websearch"        --> doCenterFloat
    , title     =? "urxvt_float"      --> doSideFloat SC
    , isFullscreen                    --> doFullFloat
    , isDialog                        --> doCenterFloat
    , stringProperty "WM_NAME" =? "LINE" --> (doRectFloat $ W.RationalRect 0.60 0.1 0.39 0.82)
    , stringProperty "WM_NAME" =? "Google Keep" --> (doRectFloat $ W.RationalRect 0.3 0.1 0.4 0.82)
    , stringProperty "WM_NAME" =? "tmptex.pdf - 1/1 (96 dpi)" --> (doRectFloat $ W.RationalRect 0.29 0.25 0.42 0.5)
    , stringProperty "WM_NAME" =? "Figure 1" --> doCenterFloat
    ]

myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }

-- xmobar
myWsBar = "xmobar $HOME/.xmonad/xmobarrc"

wsPP = xmobarPP { ppOrder           = \(ws:l:t:_)  -> [ws,l,t]
                , ppCurrent         = xmobarColor colorfg   colorDarkbg . \s -> "="++s++"="
                , ppUrgent          = xmobarColor colorRed  colorDarkbg . \s -> "~"++s++"~"
                , ppVisible         = xmobarColor colorfg   colorDarkbg . \s -> "-"++s++"-"
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

myMouse x = [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))

-- vim: ft=haskell
