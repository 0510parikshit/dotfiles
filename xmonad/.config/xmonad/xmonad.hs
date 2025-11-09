import XMonad
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import System.Exit

import XMonad.Actions.ToggleFullFloat
import XMonad.Actions.CycleWS
import XMonad.Actions.Commands

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.NamedScratchpad
import XMonad.Util.Dmenu (dmenu)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.RefocusLast (refocusLastLogHook)

import XMonad.Layout
import XMonad.Layout.NoBorders



myTerm :: String
myTerm = "alacritty --title term"

-- myMenu :: String
-- myMenu = "dmenu_run -b -fn JetBrainsMonoNF -nb '#363a4f' -nf '#cad3f5'"

myMenu :: String
myMenu = "rofi -show combi -modes 'window,drun,run,combi'"


myPowerMenu :: String
myPowerMenu = "/home/parth/.config/rofi/powermenu.sh"

main :: IO ()
main = xmonad 
     . toggleFullFloatEwmhFullscreen
     . ewmhFullscreen 
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask            = mod4Mask
    , logHook            = refocusLastLogHook
                        >> nsHideOnFocusLoss myScratchpads
    , layoutHook         = smartBorders $ avoidStruts $ myLayout
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , borderWidth        = 3
    , focusedBorderColor = "#b7bdf8"
    , normalBorderColor  = "#000000"
    }
  `additionalKeysP`
    [ ("M-f"                     , withFocused toggleFullFloat                             )
    , ("M-p"                     , spawn       myMenu                                      )
    , ("M-S-<Return>"            , spawn       myTerm                                      )
    , ("M-l"                     , spawn       "betterlockscreen -l blur"                  )
    , ("M-m k"                   , spawn       "playerctl -p spotify play-pause"           )
    , ("M-m j"                   , spawn       "playerctl -p spotify previous"             )
    , ("M-m l"                   , spawn       "playerctl -p spotify next"                 )
    , ("<XF86MonBrightnessUp>"   , spawn       "brillo -q -u 500000 -A 5"                  )
    , ("<XF86MonBrightnessDown>" , spawn       "brillo -q -u 500000 -U 5"                  )
    , ("<XF86AudioRaiseVolume>"  , spawn       "amixer sset Master 5%+"                    )
    , ("<XF86AudioLowerVolume>"  , spawn       "amixer sset Master 5%-"                    )
    , ("<XF86AudioMute>"         , spawn       "amixer sset Master toggle"                 )
    , ("C-<Space>"               , spawn       "dunstctl close-all"                        )

    , ("M-m M-m"                 , namedScratchpadAction myScratchpads "spotify"           )
    , ("M-m m"                   , namedScratchpadAction myScratchpads "spotify"           )
    , ("M-r"                     , namedScratchpadAction myScratchpads "ranger"            )
    , ("M-m h"                   , namedScratchpadAction myScratchpads "htop"              )

    , ("M-<Tab>"                 , toggleWS' ["NSP"]     )
    , ("M-<Right>"               , nextWS                )
    , ("M-<Left>"                , prevWS                )
    , ("M-S-<Right>"             , shiftToNext >> nextWS )
    , ("M-S-<Left>"              , shiftToPrev >> prevWS )

    , ("M-o"                     , spawn myPowerMenu     )
    ]


myCommands :: X [(String, X ())]
myCommands = do
           return $ otherCommands
      where
           otherCommands = 
                [ ("Log Out"       , io exitSuccess     )
                , ("PowerOff"      , sendMessage Expand )
                , ("Reboot"        , sendMessage Expand )
                ]


myScratchpads = [
                NS "spotify" "spotify-launcher" (className =? "Spotify") myFloat,
                NS "ranger" "alacritty --title ranger -e ranger" (title =? "ranger") bigFloat,
                NS "htop" "alacritty --title htop -e htop" (title =? "htop") myFloat
                ]
              where
                myFloat  = doRectFloat (W.RationalRect 0.1 0.1 0.8 0.8)
                bigFloat = doRectFloat (W.RationalRect 0.025 0.075 0.95 0.88)


myLayout = Tall 1 (3/100) (1/2) ||| Full


myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"                                --> myFloat
    , className =? "mpv"                                 --> myFloat
    , className =? "Pqiv"                                --> myFloat
    , className =? "Thunar"                              --> myFloat
    , isDialog                                           --> myFloat
    , (className =? "firefox" <&&> resource =? "Dialog") --> myFloat
    ] <+> namedScratchpadManageHook myScratchpads
  where
    myFloat = doRectFloat (W.RationalRect 0.1 0.1 0.8 0.8)


myStartupHook :: X ()
myStartupHook = do
  spawnOnce "copyq"
  spawnOnce "dunst"
  spawnOnce "stalonetray"
  spawnOnce "xsetroot -cursor_name left_ptr"


myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = lowWhite " | "
    --, ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = white . wrap " " "" . xmobarBorder "Top" "#8be9fd" 3
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta   . ppWindow
    formatUnfocused = wrap (lowWhite "") (lowWhite "")   . lowWhite  . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 20

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#f5bde6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#6e738d" ""
