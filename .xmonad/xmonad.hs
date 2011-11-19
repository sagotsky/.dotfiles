import XMonad 
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio
import XMonad.Operations
import XMonad.Config
import XMonad.Util.Run

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
--import XMonad.Layout.Magnifier
--import XMonad.Layout.Circle
--import XMonad.Layout.ResizableTile
--import XMonad.Layout.Spiral
import XMonad.Layout.IM
import XMonad.Layout.ShowWName
import XMonad.Layout.PerWorkspace
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Actions.CycleWS

myTerminal = "x-terminal-emulator"

myBorderWidth = 2

myModMask = mod4Mask -- use super as meta

myWorkspaces = ["web-1", "music-2", "email-3", "term-4", "chat-5", "zend-6", "7", "8", "9"] 

myNormalBorderColor = "#444455"

myFocusedBorderColor = "#cfb000"

myUrgentBorderColor = "#ff5500"

myShowWName = showWName' defaultSWNConfig
	{ swn_color = myFocusedBorderColor
	, swn_bgcolor = myNormalBorderColor
	, swn_font = "xft:Verdana:pixelsize=50:antialias=true"
	, swn_fade = 0.5 
	}

myLayout =   -- ewmhDesktopsLayout
            onWorkspace "music-2" wide $
            avoidStruts
           ( smartBorders
           ( tall ||| wide ||| Grid |||  Full ))
    where
      tall = Tall nmaster delta ratio
      wide = avoidStruts $ named "Wide" (Mirror tall)
      nmaster = 1
      delta = 3/100
      ratio = 1/2
      -- imLayout = named "chat" $ (withIM (11/64) imApps $ Grid  )
      -- imApps = Or (ClassName "Skype.real") (Role "contact_list")
--           $ layoutHook defaultConfig


myManageHook =  composeAll
    [ 
     className =? "MPlayer"        --> doFloat
     ,className =? "xine"        --> doFullFloat
     --,className =? "totem"        --> doFloat
    --, className =? "Gimp"           --> doFloat
    , className =? "Gnome-calculator"           --> doFloat
    , resource  =? "Do" --> doIgnore

    , stringProperty "WM_WINDOW_ROLE" =? "browser"    --> doShift "web-1"
    --, className =? "Firefox"          --> doShift "web-1" 
    , className =? "Namoroka"          --> doShift "web-1" 
    , className =? "Rhythmbox"        --> doShift "music-2" 
    , className =? "Thunderbird"  --> doShift "email-3" 
    , className =? "Pidgin"  --> doShift "chat-5" 
    , className =? "Skype.real"  --> doShift "chat-5" 
    , className =? "xterm-mail" --> doShift "email-3"
    -- the above is achieved by "xterm -class xterm-mail"
    , className =? "Operapluginwrapper-ia32-linux" --> doFullFloat
    , className =? "Exe" --> doFullFloat
    , className =? "Zend Studio" --> doShift "zend-6" 

    --, isFullsreen                                       --> myDoFullFloat
    --myDoFullFloat :: ManageHook
    --myDoFullFloat = doF W.focusDown <+> doFullFloat
    ] 


-- myStartupHook = return ()
-- tells java programs that they can work in this wm.
--myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D"


main = do 
  xmproc <- spawnPipe "xmobar"
  sp <- mkSpawner
  xmonad $ withUrgencyHook NoUrgencyHook
       $ ewmh defaultConfig
             {terminal = myTerminal
             --startupHook = myStartupHook
             ,borderWidth = myBorderWidth
             ,modMask = myModMask
             ,workspaces = myWorkspaces
             ,normalBorderColor = myNormalBorderColor
             ,focusedBorderColor = myFocusedBorderColor
--             ,urgentBorderColor = myUrgentBorderColor
             --,keys = myKeys
                     
             ,layoutHook = myShowWName myLayout
             ,manageHook = manageSpawn sp 
                            <+> manageDocks <+> myManageHook 
                           <+> manageHook defaultConfig
             ,logHook = dynamicLogWithPP $ xmobarPP 
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor  "white" "" . shorten 80 . wrap " " " "
                         , ppUrgent = xmobarColor myUrgentBorderColor "" . wrap "◄ "  " ►"
                         , ppCurrent = xmobarColor myFocusedBorderColor "" . wrap "<" ">"
                         , ppVisible = xmobarColor myFocusedBorderColor "" . wrap "" ""
                         , ppLayout  = xmobarColor "#aaaaaa" "" . wrap "" ""  
                         , ppSep =  " · " 
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
	  ("<XF86HomePage>", spawn "nautilus --no-desktop") --home
	, ("<XF86Favorites>", spawn "/home/sagotsky/scripts/gdmSwitch.sh jenn") --user switch
	, ("<XF86Search>", spawn "xcalib -a -i") -- screen color invert
    , ("<XF86Mail>", spawn "/home/sagotsky/scripts/active_win_man.sh") -- manpage for active win
	, ("<XF86Calculator>", spawn "gnome-calculator") --calc
    , ("M-x", spawn "dmenu_run -b -i -m 0 -fn -*-lucida-bold-r-*-*-16-*-*-*-*-*-*-* -sb '#cfb000' -sf '#000' -nf '#fff' -nb '#4a525a'")
    , ("M-b", spawn "/home/sagotsky/scripts/wallpaper.sh") -- dmenu for jumping rhythmbox songs

    -- music
    , ("<XF86AudioPlay>", spawn "rhythmbox-client --play-pause") --vol up
	, ("<XF86Forward>", spawn "rhythmbox-client --next") --Next
	, ("<XF86Back>", spawn "rhythmbox-client --previous") --Back
	, ("<XF86AudioRaiseVolume>", spawn "/home/sagotsky/scripts/vol-up.sh") --vol up
	, ("<XF86AudioLowerVolume>", spawn "/home/sagotsky/scripts/vol-down.sh") --vol down
	, ("<XF86AudioMute>", spawn "/home/sagotsky/scripts/vol-mute.sh") --vol mute
	, ("<XF86LaunchA>", spawn "rhythmbox-client --set-rating 1") -- rate 1-5
	, ("<XF86LaunchB>", spawn "rhythmbox-client --set-rating 2") -- rate 1-5
	, ("<XF86LaunchC>", spawn "rhythmbox-client --set-rating 3") -- rate 1-5
	, ("<XF86LaunchD>", spawn "rhythmbox-client --set-rating 4") -- rate 1-5
	, ("<XF86LaunchE>", spawn "rhythmbox-client --set-rating 5") -- rate 1-5
    , ("M-m",   spawn "/home/sagotsky/scripts/rb-jump.sh") -- dmenu for jumping rhythmbox songs
    , ("M-S-m", spawn "/home/sagotsky/scripts/rb-jump.sh -e") -- dmenu for jumping rhythmbox songs, enqueue
    , ("M-C-m", spawn "/home/sagotsky/scripts/rb-jump.sh -d") -- dmenu for jumping rhythmbox songs, eneuque dir

    -- transparency
    ,("M-S-o", spawn "transset-df -a --inc 0.1")
    ,("M-o",   spawn "transset-df -a --dec 0.1")

    -- DynamicWorkspaces 
    , ("M-d",   removeWorkspace )
    , ("M-'",   selectWorkspace myXPConfig )
    , ("M-S-'", withWorkspace myXPConfig (windows . W.shift) )

    --CycleWS
    , ("M-0", toggleWS)
	]


myXPConfig = defaultXPConfig 
                { bgColor = "#4a525a"
                , fgColor = "#ffffff"
                , fgHLight = "#000000"
                , bgHLight = myFocusedBorderColor 
                , borderColor = myFocusedBorderColor -- "#000000"
                , promptBorderWidth = 1
                , height = 35
                , font = "xft:Verdana:pixelsize=20:antialias=true"
                
                }

-- you get no guarantee that this works correctly!
--currentNumWindows :: X Int
--currentNumWindows = do
    --ws <- gets windowset
    --return $ case ( W.stack $ W.workspace $ W.current ws ) of
        --(Just s) -> (length $ W.up s) + (length $ W.down s) + (M.size $ W.floating ws) + 1
        --otherwise -> 0

-- intToStr ::  Int -> String
-- intToStr i = show i

