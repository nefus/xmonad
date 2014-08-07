import Data.Map as M (fromList,union, Map())
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.StackSet as W (focusUp, focusDown, sink)
import XMonad.Util.Dmenu
import XMonad.Util.Run(spawnPipe)
import System.Exit
import Control.Monad
import Data.Ratio
import qualified Data.Map as M

confirmQuit :: X()
confirmQuit = do
	let m = "confirm quit"
	s <- dmenu [m]
	when (m == s) (spawn "xfce4-session-logout")


myLayouts = avoidStruts (resizable_tall ||| Mirror (resizable_tall) ||| noBorders Full) 
	where
--	tall = Tall num_master scroll_step (17 % 32)
	tall_eq = Tall num_master scroll_step (1 % 2)
	resizable_tall = ResizableTall num_master scroll_step frac []
	num_master = 1
	scroll_step = 2 % 100
	spiral_ratio = 3 % 4
	frac = 1 % 2

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
	     [ 	  ((modm .|. shiftMask, xK_l  ), spawn "xscreensaver-command -lock")
		, ((modm              , xK_y  ), sendMessage MirrorShrink)
		, ((modm              , xK_a  ), sendMessage MirrorExpand)
		, ((modm .|. shiftMask, xK_g  ), gotoMenu)
		, ((modm .|. shiftMask, xK_b  ), bringMenu)
		, ((modm              , xK_b  ), sendMessage ToggleStruts)
		, ((modm              , xK_plus), spawn "amixer set Master 5%+")
		, ((modm              , xK_minus), spawn "amixer set Master 5%-")
		, ((modm .|. shiftMask, xK_p),   spawn "dmenu_run")
		, ((modm .|. shiftMask, xK_q),   confirmQuit)
		, ((modm	      , xK_v),   windows copyToAll) -- @@ Make focused window always visible
		, ((modm .|. shiftMask, xK_v),   killAllOtherCopies)  -- @@ Toggle window state back
             ]
myManageHook = composeAll [
	className =? "Firefox" --> doShift "1",
	className =? "Thunderbird" --> doShift "1",
	className =? "Eclipse" --> doShift "2",
	className =? "Pidgin"  --> doShift "7"
	]

main = do
--	xmproc <- spawnPipe "/usr/bin/xmobar --screen=0 ~/.xmonad/xmobarrc.hs"
	xmonad defaultConfig
		{ keys = myKeys <+> keys defaultConfig
		, modMask = mod4Mask
		-- , terminal = "xterm -fn terminus-16"
		, terminal = "x-terminal-emulator"
		, handleEventHook = ewmhDesktopsEventHook
		, layoutHook = myLayouts
		, logHook = ewmhDesktopsLogHook
		, manageHook = manageDocks <+> myManageHook <+>  manageHook defaultConfig
		, startupHook = ewmhDesktopsStartup
		}

