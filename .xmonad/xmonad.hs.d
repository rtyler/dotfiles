{- xmonad.hs
 - Compiled from many sources, thank you.
 -}

-- Import stuff
import XMonad
import System.Exit
import System.IO
import Graphics.X11.Xlib
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.IM
import XMonad.Actions.CycleWS
import Data.Ratio ((%))
import Data.List
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- General Settings
myTerminal      = "urxvt"
myFocusFollowsMouse = True
myBorderWidth   = 1
myModMask       = mod4Mask -- The Super Key
altMask         = mod1Mask  -- The Left Alt
myNumlockMask   = mod2Mask -- The Right Alt

-- Font
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859-*"

-- Paths
myBitmapsPath = "/home/demizer/.dzen2/"

-- Colors
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#004D99"
myDzenFGColor = "#0296F8"
myDzenBGColor = "#CCCCCC"
myNormalFGColor = "#FFFFFF"
myNormalBGColor = "#0F0F0F"
myFocusedFGColor = "#F0F0F0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099FF"
myUrgentBGColor = "#FF0000"
myUrgencyHintFGColor = "#FFFFFF"
myUrgencyHintBGColor = "#FFFFFF"
mySeperatorColor = "#555555"
myLayoutNameColor = "#D0F0C0"
myHiddenFGColor = "#CFCFCF"
myHiddenNoWindowsFGColor = "#666666"
myCurrentFGColor = "#0296F8"
myCurrentBGColor = "#000000"
myTitleFGColor = "#F88379"
myWsSepFGColor = "#3B444B"

-- Bars
-- dzen general options
myDzenGenOpts = "-fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -h '25'"
myStatusBar = "dzen2 -w 960 -ta l " ++ myDzenGenOpts

-- Conky Bar
myConkyBarTopRight = "conky -c /home/demizer/.conky.top.right | sh | dzen2 -p -ta r -x 960 -w 960 " ++ myDzenGenOpts
myConkyBarBotRight = "conky -c /home/demizer/.conky.bot.right | sh | dzen2 -p -ta r -x 960 -y 1080 -w 960 " ++ myDzenGenOpts
myConkyBarBotLeft  = "conky -c /home/demizer/.conky.bot.left  | sh | dzen2 -p -ta l -y 1080 -w 960 " ++ myDzenGenOpts

-- Tabbed Layout Theme
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    --, decoWidth = ""
    --, decoHeight = ""
    }

-- Dzen config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent = dzenColor myCurrentFGColor myCurrentBGColor . pad
    , ppVisible = dzenColor myNormalFGColor "" . pad
    , ppHidden  = dzenColor myHiddenFGColor "" . pad
    , ppHiddenNoWindows = dzenColor myHiddenNoWindowsFGColor  "" . pad
    , ppUrgent  = dzenColor "" myUrgentBGColor
    , ppWsSep    = dzenColor myWsSepFGColor "" "Ξ"
    , ppSep      = "|"
    , ppLayout   = dzenColor myLayoutNameColor "" .
              (\ x -> case x of
                  "Maximize Tall"           -> "[]="
                  "Maximize Mirror Tall"    -> "TTT"
                  "Maximize Full"           -> "<M>"
                  "Maximize Grid"           -> "+++"
                  "Maximize Spiral"         -> "(@)"
                  "Maximize Accordion"      -> "Acc"
                  "Maximize Tabbed Simplest"        -> "Tab"
                  "Maximize Tabbed Bottom Simplest" -> "TaB"
                  "Maximize SimplestFloat"          -> "><>"
                  "Maximize IM"             -> "IM "
                  "Maximize Dishes 2 (1%6)" -> "Dsh"
                  _                         -> pad x
              )
    , ppTitle    =  (" " ++) . dzenColor myTitleFGColor "" . shorten 120 . dzenEscape
    , ppOutput   = hPutStrLn h
    }

--LayoutHook
myLayoutHook  = onWorkspace "1:web"   webL  $ onWorkspace "2:code"  codL
              $ onWorkspace "6:gimp"  gimpL $ onWorkspace "7:games" full $ standardLayouts
    where
    standardLayouts = avoidStruts $ tiled ||| Mirror tiled ||| full
    tiled           = Tall nmaster delta ratio
    tabLayout       = (tabbed shrinkText myTheme)
    full            = Full
    gimpL           = avoidStruts $ smartBorders
                      $ withIM (0.11) (Role "gimp-toolbox")
                      $ reflectHoriz
                      $ withIM (0.15) (Role "gimp-dock") full
    {- webL            = tabLayout  ||| tiled ||| reflectHoriz tiled |||  full
    -}
    filL            = avoidStruts $ tiled ||| reflectHoriz tiled ||| full
    webL            = avoidStruts $ tabLayout ||| full
    codL            = avoidStruts $ tiled ||| reflectHoriz tiled ||| full
    nmaster         = 1
    delta           = 3/100
    ratio           = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio

-- Workspaces
myWorkspaces = ["1:web", "2:code", "3:file", "4:media", "5:chat", "6:gimp" ,"7:games", "8", "9"]

-- Window rules:
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [fmap (t `isInfixOf`) title --> doCenterFloat | t <- myTFloats]
    , [resource =? r --> doCenterFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "1:www"   | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "2:code"  | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "3:file"  | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "4:media" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5:chat"  | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6:gimp"  | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:games" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:games" | x <- my7Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Ekiga", "MPlayer", "Nitrogen", "Nvidia-settings", "XCalc", "XFontSel", "Xmessage"]
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As...", "Wine", "VLC", "Options", "Compose Mail"]
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop", "Wine", "exe"]
    my1Shifts = ["Chromium"]
    my2Shifts = ["Emacs", "GVim"]
    my3Shifts = ["File Browser"]
    my4Shifts = ["Geeqie", "Gthumb", "Gmpc", "ncmpc", "MPlayer", "Easytag"]
    my5Shifts = ["Xchat"]
    my6Shifts = ["Gimp"]
    my7Shifts = ["Brood War", "VirtualBox", "Steam"]

-- Urgency hint configuration
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "785", "-h", "15", "-w", "1280",
         "-ta", "r", "-expand", "l",
         "-fg", "" ++ myUrgencyHintFGColor ++ "",
         "-bg", "" ++ myUrgencyHintBGColor ++ "",
         "-fn", "" ++ myFont ++ ""
         ]
    }

-- keys
myKeys :: XConfig Layout -> M.Map (XMonad.KeyMask, XMonad.KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c ), kill)

    -- launch dmenu
    , ((modMask, xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- GridSelect
    {- , ((modMask, xK_g), goToSelected defaultGSConfig) -}

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)

    -- refresh'
    , ((modMask, xK_n ), refresh)

    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )

    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)

    -- screenshot screen
    -- http://en.gentoo-wiki.com/wiki/Screenshot
    , ((modMask,               xK_Print     ), spawn "/home/demizer/bin/screenshot scr")

    -- screenshot window or area
    , ((modMask .|. shiftMask, xK_Print     ), spawn "/home/demizer/bin/screenshot win")

    -- scratchpad
    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal})

    --Programs
    , ((modMask .|.  shiftMask, xK_b ), spawn "chromium")
    , ((modMask .|.  shiftMask, xK_e ), spawn "emacs")
    , ((modMask .|.  shiftMask, xK_v ), spawn "gvim")
    , ((modMask .|.  shiftMask, xK_n ), spawn "nautilus /home/demizer")

    -- volume control
    -- {- , ((0           , 0x1008ff13 ), spawn "amixer -q set Master 2dB+") -}
    -- {- , ((0           , 0x1008ff11 ), spawn "amixer -q set Master 2dB-") -}
    -- {- , ((0           , 0x1008ff12 ), spawn "amixer -q set Master toggle") -}

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    , ((modMask .|. shiftMask, xK_End), spawn "sudo halt")
    , ((modMask .|. controlMask, xK_End), spawn "sudo pm-suspend")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- The Meat
main :: IO ()
main = do
    workspaceBarPipe <- spawnPipe myStatusBar
    conkyPipeTopRight <- spawnPipe myConkyBarTopRight
    conkyPipeBotLeft <- spawnPipe myConkyBarBotLeft
    conkyPipeBotRight <- spawnPipe myConkyBarBotRight
    xmonad $ ewmh $ myUrgencyHook $ defaultConfig {

    -- simple stuff
    terminal            = myTerminal,
    focusFollowsMouse   = myFocusFollowsMouse,
    borderWidth         = myBorderWidth,
    modMask             = myModMask,
    {- numlockMask         = myNumlockMask, -}
    workspaces          = myWorkspaces,
    normalBorderColor   = myNormalBorderColor,
    focusedBorderColor  = myFocusedBorderColor,

    -- key bindings
    keys                = myKeys,

    -- hooks, layouts
    manageHook          = myManageHook <+> manageDocks,
    logHook             = myLogHook workspaceBarPipe,

    -- For use with no panels or just dzen2
    layoutHook          = smartBorders $ myLayoutHook
    }
