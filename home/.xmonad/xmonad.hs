import XMonad
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio
import Data.Traversable (traverse)
import Data.Maybe (listToMaybe, maybeToList)
import Data.List ( (\\) )

-- import Text.Regex.XMLSchema.String
import XMonad.Operations
import XMonad.Config
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutHints
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.IM
import XMonad.Layout.ShowWName
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Actions.CycleWS

myTerminal            = "x-terminal-emulator"
myBorderWidth         = 1
myModMask             = mod4Mask
myWorkspaces          = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]
myNormalBorderColor   = "#222233"
myFocusedBorderColor  = "#cfb000"
myUrgentBorderColor   = "#ff5500"

myLayout = avoidStruts
           ( smartBorders
           -- ## |- [] ()
           --( named "║" tall ||| named "═" wide ||| named "□"  Full ||| named "Ο" circle ))
           ( named "<icon=tall.xbm/>" tall ||| named "<icon=wide.xbm/>" wide ||| named "<icon=full.xbm/>"  Full ||| named "<icon=circle.xbm/>" circle ))
    where
      tall = smartSpacing 10 $ Tall nmaster delta ratio
      wide = smartSpacing 10 $ Mirror $ Tall nmaster delta ratio
      circle = layoutHints Circle
      nmaster = 1
      delta = 3/100
      ratio = 1/2


myManageHook =  composeAll
    [
     className =? "MPlayer"                           --> doFloat
    , className =? "Gnome-calculator"                 --> doFloat
    , className =? "doukutsu"                         --> doFloat
    , className =? "Zenity"                           --> doFloat
    , className =? "Peek"                             --> doFloat
    , className =? "xine"                             --> doFullFloat
    , className =? "Operapluginwrapper-ia32-linux"    --> doFullFloat
    , className =? "Exe"                              --> doFullFloat -- chrome flash
    , className =? "Plugin-container"                 --> doFullFloat -- firefox chrome flash
    , className =? "The Binding of Isaac + Wrath of the Lamb"                              --> doFullFloat
    , className =? "Gimp"                             --> (ask >>= doF . W.sink)
    , resource  =? "Do"                               --> doIgnore
    , resource  =? "feh"                              --> doIgnore
    , className =? "Unity-2d-panel"                  --> doIgnore
    , className =? "Unity-2d-launcher"               --> doIgnore
    , className =? "Zenity"                          --> doIgnore
    , className =? "Screenkey"                       --> doIgnore

    -- , stringProperty "WM_WINDOW_ROLE" =? "browser"    --> doShift "web-1"
    , className =? "Firefox"                          --> doShift "I"
    , className =? "Rhythmbox"                        --> doShift "II"
    , className =? "spotify"                          --> doShift "II"
    , className =? "Spotify"                          --> doShift "II"
    , resource =? "cmus"                              --> doShift "II"
    , className =? "Thunderbird"                      --> doShift "III"
    , className =? "Gnome-terminal"                   --> doShift "IV"
    , className =? "Pidgin"                           --> doShift "V"
    , className =? "Scudcloud"                        --> doShift "V"
    , className =? "Slack"                            --> doShift "V"
    , className =? "xterm-mail"                       --> doShift "III"
    , className =? "Zend Studio"                      --> doShift "VI"
    , className =? "Sublime_text"                     --> doShift "VI"
    , className =? "Steam"                            --> doShift "X"

    , isFullscreen --> doFullFloat
    ]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ withUrgencyHook NoUrgencyHook
       $ ewmh defaultConfig
             {terminal = myTerminal
             ,borderWidth = myBorderWidth
             ,modMask = myModMask
             ,workspaces = myWorkspaces
             ,normalBorderColor = myNormalBorderColor
             ,focusedBorderColor = "#dddddf" -- myFocusedBorderColor
             ,layoutHook = myShowWName myLayout
             , handleEventHook = handleEventHook
               defaultConfig <+> fullscreenEventHook
             ,manageHook = manageDocks <+> myManageHook
                           <+> manageHook defaultConfig
             -- http://www.alanwood.net/demos/wgl4.html special chars
             ,logHook = dynamicLogWithPP $ xmobarPP
               { ppOutput = hPutStrLn xmproc
                 , ppTitle = xmobarColor  "white" "" . shorten 140 . wrap " " " "
                 , ppCurrent = xmobarColor "#ec5500" ""
                 , ppVisible = xmobarColor "#ec5500" ""
                 , ppUrgent = xmobarColor "#ffffff" ""
                 , ppHidden =     xmobarColor "#444444" ""
                 , ppLayout  = xmobarColor "#888888" ""
                 , ppHiddenNoWindows =     xmobarColor "#222222" ""
                 , ppSep =  " "

                 -- , ppOrder = \(workspace:layout:title:extras:_) -> workspace : layout : title : [xmobarColor "#666666" "" extras]
                 -- , ppUrgent = xmobarColor "white" "" . sed (const "•") ".*[0-46-9]" . sed (const "• ") ".*5"    --- should urget be semi random?  ie #ddd-fff fluctuating or throbbing?  might be a good haskell excersie
                 -- , ppCurrent = xmobarColor "#ec5500" "" . sed (const "•") ".*[0-46-9]" . sed (const "• ") ".*5"
                 -- , ppVisible = xmobarColor "#a83300" "" .  sed (const "•") ".*[0-46-9]". sed (const "• ") ".*5"
                 -- , ppHidden =     xmobarColor "#888888" "" . sed (const "•") ".*[0-9]". sed (const "• ") ".*5"
                 -- , ppLayout  = xmobarColor "#888888" "" . wrap "" ""
                 -- , ppHiddenNoWindows =     xmobarColor "#666666" "" . sed (const "◦") ".*[0-46-9]". sed (const "◦ ") ".*5" -- replace 5 first, then general.
                 -- , ppExtras = [ logTitles ]
                 --, ppHidden  = xmobarColor "#aaaaaa" "" . wrap "" ""
                 --Current      workspace with focus
                 --Visible      displayed workspace without focus
                 --Hidden       workspace that isn't displayed
                 --HiddenNoWindows
                 --Urgent
                 --
                 --Sep string between sections
                 --WsSep    sep between ws types
                 --Title    active window title
                 --Layout   name of current layout
                 --Order
                 --Sort
                 --Extras   time and date, tertiary loggers
                 --Output   entire string
               }



             }
             `additionalKeysP` myKeys

myKeys = [
    -- application shortcuts
	  ("<XF86HomePage>",   spawn "nautilus") --home browser
    , ("<XF86Calculator>", spawn "toggle.sh gnome-calculator") --calc
    , ("M-g",              spawn "fmarks.sh") -- open FF bookmarks in current browser
    , ("M-v",              spawn "sleep .5 ; xdotool click 2") -- sleep so I can release Meta

    -- music
    , ("M-<Up>"  ,               spawn "xterm -e music-remote.sh") --vol up
    , ("M-<Down>",               spawn "music-client.sh toggle") --vol up
    , ("M-<Right>",              spawn "music-client.sh next") --Next
    , ("M-<Left>",               spawn "music-client.sh back") --Back
    , ("<XF86AudioPlay>",        spawn "music-client.sh toggle") --vol up
    , ("<XF86Forward>",          spawn "music-client.sh next") --Next
    , ("<XF86AudioNext>",        spawn "music-client.sh next") --Next
    , ("<XF86Back>",             spawn "music-client.sh back") --Back
    , ("<XF86AudioPrev>",        spawn "music-client.sh back") --Back
    , ("<XF86AudioRaiseVolume>", spawn "vol-up.sh") --vol up
    , ("<XF86AudioLowerVolume>", spawn "vol-down.sh") --vol down
    , ("<XF86AudioMute>",        spawn "vol-mute.sh") --vol mute

    -- transparency
    , ("M-o",   spawn "transset-df -a --dec 0.03") -- make transparent
    , ("M-C-o", spawn "transset-df -a --inc 0.03") -- remove transparent
    , ("M-S-o", spawn "transset-df -a --inc 1") -- reset transparent

    --xcalib screen options
    , ("<XF86Search>",   spawn "xcalib -a -i") -- screen color invert
    , ("C-<XF86Search>", spawn "xcalib -c") -- screen color reset
    , ("S-<XF86Search>", spawn "xcalib -a -co 95") -- screen contrast decrease
    , ("M-<XF86Search>", spawn "xcalib -a -b   5") -- screen brightness increase

    -- WM Shortcuts
    , ("M-x",   spawn "dmenu_run -i ") -- $path launcher
    , ("M-S-b", spawn "wallpaper.sh") -- swap wallpaper
    , ("M-C-b", spawn "wallpaper.sh new") -- newer wallpaper
    , ("M-S-C-b", spawn "wallpaper.sh old") -- older wallpaper
    , ("M-b",   sendMessage ToggleStruts) -- struts are panels.
    , ("M-S-d",   removeWorkspace ) -- Delete active workspace
    , ("M-'",   selectWorkspace myXPConfig ) -- Create workspace
    , ("M-S-'", withWorkspace myXPConfig (windows . W.shift) ) -- Shift win to named workspace
    , ("M-S--", swapNextScreen) -- Swap screens - CycleWS
    , ("M--",   toggleWS)  -- Goto previous screen (cd -) - CycleWS
    , ("M-S-u", spawn "toggle.sh trayer --align left --width 50% --height 32") -- show tray
    , ("M-u",   spawn "toggle.sh `cat ~/.panel || echo gnome-panel` ; dzen-clear.sh") -- show panel
    , ("M-0",   windows $ W.greedyView "X")  -- workspace 0
    , ("M-S-0", (windows $ W.shift "X") >> (windows $W.greedyView "X")) -- shift window to WS 0
    , ("M-;",   spawn "cheese.sh") -- center mouse on active window
    , ("M-S-q", return ()) -- don't you fucking quit.  that's what ctrl-alt-backsp is for.

    -- misc scripts
    , ("M-y",   spawn "cli-board.sh") -- copies text into clip board
    , ("M-S-y", spawn "cheat-sheet.sh") -- views files in .cheat-sheets
    , ("M-<Escape>", spawn "bender-dock.sh") -- reset screen, xmodmap, etc
    , ("M-d", spawn "notify-send Date \"`date`\" ") -- show date on current screen.
	]





myXPConfig = defaultXPConfig
                { bgColor = "#101015"
                , fgColor = "#ffffff"
                , fgHLight = "#101015"
                , bgHLight = "white"
                , borderColor = "#000000"
                , promptBorderWidth = 1
                , height = 35
                , font = "xft:Verdana:pixelsize=20:antialias=true"

                }

myShowWName = showWName' defaultSWNConfig
	{ swn_bgcolor = "#101015"
	, swn_color = "white"
	, swn_font = "xft:Verdana:pixelsize=50:antialias=true"
	, swn_fade = 0.5
	}

-- if they're all here we could regex them into one char each, showing window count
--logTitles :: X (Maybe String) -- this is a Logger
--logTitles = withWindowSet $ fmap (Just . unwords) -- fuse the window names
  -- . traverse (fmap show . getName) -- show window names
  -- .  tail . W.index -- all windows except master

-- logTitles :: X (Maybe String) -- this is a Logger
-- logTitles = withWindowSet $ fmap (Just . unwords) -- fuse window names
