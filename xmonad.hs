import Data.Ratio
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer
import XMonad.Config.Xfce
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.Hidden
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.StackSet as W
import XMonad.Util.Dmenu

import qualified Data.Map as M


main = xmonad $ ewmhFullscreen $ ewmh xfceConfig {
    modMask = mod4Mask
    , keys = myKeys <+> keys def
    , layoutHook = myLayouts
    , focusedBorderColor = "#960000" 
    , focusFollowsMouse = True
    , manageHook = manageDocks <+> myManageHook
    , normalBorderColor = "#000000"
    , terminal = "x-terminal-emulator"    
}

-- myLayouts = hiddenWindows (avoidStruts $ id
--         . mkToggle (single  FULL)
--         . mkToggle (single MIRROR)
--         $ resizable_tall) 
myLayouts = avoidStruts $ id
        . mkToggle (single  FULL)
        . mkToggle (single MIRROR)
        $ resizable_tall        
    ||| noBorders Full
      where
        resizable_tall = ResizableTall num_master scroll_step frac []
        num_master = 1
        scroll_step = 2 % 100
        frac = 3 % 5

myManageHook = composeAll $
    [ isFullscreen --> doFullFloat
      , isDialog --> doCenterFloat
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
    [     ((modm .|. shiftMask, xK_l), spawn "slock")
--        , ((modm              , xK_x), withFocused hideWindow)
--        , ((modm .|. shiftMask, xK_x), popNewestHiddenWindow)
        , ((modm              , xK_y), sendMessage MirrorShrink)
        , ((modm              , xK_a), sendMessage MirrorExpand)
        , ((modm .|. shiftMask, xK_g), gotoMenu)
        , ((modm .|. shiftMask, xK_b), bringMenu)
        -- , ((modm .|. shiftMask, xK_b), spawn "rofi -show window")
        , ((modm              , xK_b), sendMessage ToggleStruts) -- @@ hide/unhide bar
        , ((modm              , xK_f), sendMessage $ Toggle FULL)
        , ((modm              , xK_m), sendMessage $ Toggle MIRROR)
        , ((modm .|. shiftMask, xK_p), spawn "rofi -modi drun,run,window,ssh -show drun")
        , ((modm .|. shiftMask, xK_q), io exitSuccess)
        , ((modm              , xK_v), windows copyToAll) -- @@ Make focused window always visible
        , ((modm .|. shiftMask, xK_v), killAllOtherCopies)  -- @@ Toggle window state back
        , ((0, xK_Print), spawn "flameshot launcher")
    ]

-- vim: ts=4 sw=4 et ai syntax=haskell
