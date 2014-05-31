import XMonad 
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio
import Data.Traversable (traverse)
import Data.Maybe (listToMaybe, maybeToList)
import Data.List ( (\\) )

import Text.Regex.XMLSchema.String
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
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Actions.CycleWS

myTerminal            = "x-terminal-emulator"
myBorderWidth         = 2
myModMask             = mod4Mask 
myWorkspaces          = ["web-1", "music-2", "email-3", "term-4", "chat-5", "ide-6", "7", "8", "9", "0"] 
myNormalBorderColor   = "#444455"
myFocusedBorderColor  = "#cfb000"
myUrgentBorderColor   = "#ff5500"

myLayout = onWorkspace "chat-5" circle $
            avoidStruts
           ( smartBorders
           -- ## |- [] ()
           ( named "║" tall ||| named "═" wide ||| named "□"  Full ||| named "Ο" circle ))
    where
      tall = Tall nmaster delta ratio
      wide = Mirror $ Tall nmaster delta ratio
      circle = layoutHints Circle
      nmaster = 1
      delta = 3/100
      ratio = 1/2


myManageHook =  composeAll
    [ 
     className =? "MPlayer"                           --> doFloat
    , className =? "Gnome-calculator"                 --> doFloat
    , className =? "doukutsu"                         --> doFloat
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

    -- , stringProperty "WM_WINDOW_ROLE" =? "browser"    --> doShift "web-1"
    , className =? "Firefox"                          --> doShift "web-1" 
    , className =? "Rhythmbox"                        --> doShift "music-2" 
    , resource =? "cmus"                              --> doShift "music-2" 
    , className =? "Thunderbird"                      --> doShift "email-3" 
    , className =? "Pidgin"                           --> doShift "chat-5" 
    , className =? "Skype"                            --> doShift "chat-5" 
    , className =? "Skype.real"                       --> doShift "chat-5" 
    , className =? "xterm-mail"                       --> doShift "email-3"
    , className =? "Zend Studio"                      --> doShift "ide-6" 
    , className =? "Sublime_text"                     --> doShift "ide-6" 
    ] 

main = do 
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook
       $ ewmh defaultConfig
             {terminal = myTerminal
             ,borderWidth = myBorderWidth
             ,modMask = myModMask
             ,workspaces = myWorkspaces
             ,normalBorderColor = myNormalBorderColor
             ,focusedBorderColor = "#dddddf" -- myFocusedBorderColor
             ,layoutHook = myShowWName myLayout
             ,manageHook = manageDocks <+> myManageHook 
                           <+> manageHook defaultConfig
             ,logHook = dynamicLogWithPP $ xmobarPP 
             -- http://www.alanwood.net/demos/wgl4.html special chars
               { ppOutput = hPutStrLn xmproc
                 , ppTitle = xmobarColor  "white" "" . shorten 140 . wrap " " " "
                 , ppUrgent = xmobarColor "white" "" . sed (const "•") ".*[0-46-9]" . sed (const "• ") ".*5"    --- should urget be semi random?  ie #ddd-fff fluctuating or throbbing?  might be a good haskell excersie
                 , ppCurrent = xmobarColor "#ec5500" "" . sed (const "•") ".*[0-46-9]" . sed (const "• ") ".*5"
                 , ppVisible = xmobarColor "#a83300" "" .  sed (const "•") ".*[0-46-9]". sed (const "• ") ".*5"
                 , ppHidden =     xmobarColor "#888888" "" . sed (const "•") ".*[0-9]". sed (const "• ") ".*5"
                 , ppLayout  = xmobarColor "#444444" "" . wrap "" ""  
                 , ppHiddenNoWindows =     xmobarColor "#666666" "" . sed (const "◦") ".*[0-46-9]". sed (const "◦ ") ".*5" -- replace 5 first, then general.
                 , ppSep =  " " 
                 , ppExtras = [ logTitles ]
                 , ppOrder  = \(ws:l:t:ts:_) -> ws : l : t : [xmobarColor "#666666" "" ts]
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
    , ("<XF86Favorites>",  spawn "~/scripts/gdm3switch.sh jenn") --user switch
    , ("<XF86Mail>",       spawn "active_win_man.sh") -- manpage for active win
    , ("<XF86Calculator>", spawn "gnome-calculator") --calc
    , ("M-g",              spawn "fmarks.sh") -- open FF bookmarks in current browser
    , ("M-C-<Return>",     spawn "urxvt") -- urxvt alternate term is transparent
    , ("M-s",              spawn "HOST=$(grep '^host ' .ssh/config | cut -f 2 -d' ' | ~/scripts/dmenu_hist.sh ssh -l 10 -i -s 0 -sb '#cfb000' -sf '#000' -nf '#fff' -nb '#4a525a' -fn -*-terminus-bold-r-*-*-16) && xterm -e 'ssh $HOST' ")  
    , ("M-S-s",            spawn "xterm -e stage_ssh.sh $(stage_ssh.sh | dmenu -l 10 -i -s 0 -fn terminus-bold-16 )")  
    , ("M-v",              spawn "gvim -c 'cd ~/repos/plm'") 


    -- music
    , ("M-<Up>"  ,               spawn "xterm -e music-remote.sh") --vol up
    , ("M-<Down>",               spawn "music-client.sh toggle") --vol up
    , ("M-<Right>",              spawn "music-client.sh next") --Next
    , ("M-<Left>",               spawn "music-client.sh back") --Back
    , ("<XF86AudioPlay>",        spawn "music-client.sh toggle") --vol up
    , ("<XF86Forward>",          spawn "music-client.sh next") --Next
    , ("<XF86Back>",             spawn "music-client.sh back") --Back
    , ("<XF86AudioRaiseVolume>", spawn "vol-up.sh") --vol up
    , ("<XF86AudioLowerVolume>", spawn "vol-down.sh") --vol down
    , ("<XF86AudioMute>",        spawn "vol-mute.sh") --vol mute
    , ("<XF86LaunchA>",          spawn "music-client.sh rate1") -- rate 1-5
    , ("<XF86LaunchB>",          spawn "music-client.sh rate2") -- rate 1-5
    , ("<XF86LaunchC>",          spawn "music-client.sh rate3") -- rate 1-5
    , ("<XF86LaunchD>",          spawn "music-client.sh rate4") -- rate 1-5
    , ("<XF86LaunchE>",          spawn "music-client.sh rate5") -- rate 1-5
    , ("M-m", spawn "cmus-filter.sh") -- cmus play song
    , ("M-S-m", spawn "cmus-filter.sh --list album") -- cmus play album
    , ("M-C-m", spawn "cmus-filter.sh --randomize --list album") -- cmus random album
    , ("M-M1-m", spawn "cmus-filter.sh --list artist") -- cmus artist (M1 = alt)
    , ("M-M1-C-m", spawn "cmus-filter.sh --list artist --randomize ") -- cmus artist

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
    , ("M-x",   spawn "dmenu_run -b -i -s 0 ") -- $path launcher
    , ("M-S-x", spawn "$(xdmenug.py)") -- xdg-menu launch
    , ("M-S-b", spawn "wallpaper.sh") -- swap wallpaper
    , ("M-C-b", spawn "wallpaper.sh new") -- newer wallpaper
    , ("M-S-C-b", spawn "wallpaper.sh old") -- older wallpaper
    , ("M-b",   sendMessage ToggleStruts) -- struts are panels. 
    , ("M-d",   removeWorkspace ) -- Delete active workspace
    , ("M-'",   selectWorkspace myXPConfig ) -- Create workspace
    , ("M-S-'", withWorkspace myXPConfig (windows . W.shift) ) -- Shift win to named workspace
    , ("M-S--", swapNextScreen) -- Swap screens - CycleWS
    , ("M--",   toggleWS)  -- Goto previous screen (cd -) - CycleWS
    , ("M-S-u", spawn "toggle.sh trayer --align left --width 50% --height 32") -- show tray
    , ("M-u",   spawn "toggle.sh `cat ~/.panel || echo gnome-panel` ; dzen-clear.sh") -- show panel
    , ("M-0",   windows $ W.greedyView "0")  -- workspace 0
    , ("M-S-0", (windows $ W.shift "0") >> (windows $W.greedyView "0")) -- shift window to WS 0
    , ("M-;",   spawn "cheese.sh") -- center mouse on active window

    -- misc scripts
    , ("M-/",   spawn "ri-menu.sh") -- rails docs menu
    , ("M-y",   spawn "cli-board.sh") -- copies text into clip board
    , ("M-S-y", spawn "cheat-sheet.sh") -- views files in .cheat-sheets
	]

    {-
     -- available keys
     
     , ("M-a",  spawn "")
     , ("M-c",  spawn "")
     , ("M-f",  spawn "")
     , ("M-i",  spawn "")
     , ("M-v",  spawn "")
     , ("M-z",  spawn "")
    -}
     



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

logTitles :: X (Maybe String) -- this is a Logger
logTitles = withWindowSet $ fmap (Just . unwords) -- fuse window names
  . traverse (fmap show . getName) -- show window names
  . (\ws -> W.index ws \\ maybeToList (W.peek ws))
