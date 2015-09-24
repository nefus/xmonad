import Data.Map as M (fromList,union, Map())
import Data.List
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Commands
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.StackSet as W (focusUp, focusDown, sink)
import XMonad.Util.Dmenu
--import XMonad.Util.Run(spawnPipe, unsafeSpawn)
import XMonad.Util.Run
import System.Exit
import Control.Monad
import Data.Ratio
import qualified Data.Map as M

confirmQuit :: X()
confirmQuit = do
	let m = "confirm quit"
	s <- dmenu [m]
	when (m == s) (io exitSuccess)
--	when (m == s) (spawn "xfce4-session-logout")


myLayouts = avoidStruts (noBorders Full ||| resizable_tall ||| Mirror (resizable_tall) ||| Mirror (twopane) ) 
	where
--	tall = Tall num_master scroll_step (17 % 32)
	twopane = TwoPane (3/100) (2/3)	
	tall_eq = Tall num_master scroll_step (1 % 2)
	resizable_tall = ResizableTall num_master scroll_step frac []
	num_master = 1
	scroll_step = 2 % 100
	spiral_ratio = 3 % 4
	frac = 1 % 2

commands :: X [(String, X())]
commands = defaultCommands

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
	     [ 	 -- ((modm .|. shiftMask, xK_l  ), spawn "slock")
		((modm .|. shiftMask, xK_l  ), spawn "xscreensaver-command -lock")
		, ((modm              , xK_y  ), sendMessage MirrorShrink)
		, ((modm              , xK_a  ), sendMessage MirrorExpand)
		, ((modm .|. shiftMask, xK_g  ), gotoMenu)
		, ((modm .|. shiftMask, xK_b  ), bringMenu)
		, ((modm              , xK_b  ), sendMessage ToggleStruts)
		, ((modm              , xK_plus), spawn "amixer set Master 5%+")
		, ((modm              , xK_minus), spawn "amixer set Master 5%-")
		, ((modm	      , xK_Print), unsafeSpawn "import -window root $HOME/xwd-$(date +%s)$$.png")
		, ((modm              , xK_m),   commands >>= runCommand)
		, ((modm .|. shiftMask, xK_p),   spawn "dmenu_run")
		, ((modm .|. shiftMask, xK_q),   confirmQuit)
		, ((modm	      , xK_v),   windows copyToAll) -- @@ Make focused window always visible
		, ((modm .|. shiftMask, xK_v),   killAllOtherCopies)  -- @@ Toggle window state back
		, ((modm .|. controlMask, xK_v),   spawn "xclip -o -selection clipboard")
             ]
myManageHook = composeAll [
	--className =? "Firefox" --> doShift "1",
	-- className =? "Thunderbird" --> doShift "1",
	-- className =? "Eclipse" --> doShift "2",
	--	className =? "Pidgin"  --> doShift "7"
	-- WM_WINDOW_ROLE(STRING) = "AlarmWindow"
--	isInfixOf "Reminder" title --> doFloat
	title =?? "Reminder" --> doFloat 
 	]


(=??) :: Eq a => Query [a] -> [a] -> Query Bool
q =?? x = fmap (isInfixOf x) q


main = do
--	xmproc <- spawnPipe "/usr/bin/xmobar --screen=0 ~/.xmonad/xmobarrc.hs"
	xmonad $ ewmh defaultConfig
		{ keys = myKeys <+> keys defaultConfig
		, modMask = mod4Mask
		-- , terminal = "xterm -fn terminus-16"
		, terminal = "x-terminal-emulator"
		, handleEventHook = ewmhDesktopsEventHook
		, layoutHook = myLayouts
		, logHook = ewmhDesktopsLogHook
		, manageHook = manageDocks <+> manageHook defaultConfig  <+> myManageHook 
		, startupHook = setWMName "LG3D" <+> ewmhDesktopsStartup
		}

