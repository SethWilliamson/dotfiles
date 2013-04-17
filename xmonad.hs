-- References:
-- 
-- [1] Example config
-- http://code.haskell.org/xmonad/man/xmonad.hs
--

-- Imports

import XMonad hiding ( (|||) ) 				-- don't use the normal ||| operator
import XMonad.Layout.LayoutCombinators   		-- use the one from LayoutCombinators instead
import XMonad.Prompt
import Data.Monoid
import System.Exit
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Map        as M
-- import XMonad.Util.EZConfig (additionalKeysP)

-- Import: Actions
import XMonad.Actions.DynamicWorkspaces

-- Import: Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

-- Import: Utils
import XMonad.Util.Run(spawnPipe)



-- Terminal
--myTerminal		:: String
myTerminal		= "urxvt"

-- Borders
--myBorderWidth		:: Dimension
myBorderWidth		= 3

--myNormalBorderColor	= String
myNormalBorderColor	= "#cccccc"

--myFocusedBorderColor	= String
myFocusedBorderColor	= "#008000"


-- Whether focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Key bindings
--myModMask 		:: keyMask
myModMask		= mod4Mask	-- Super Key
altMask			= mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modm}) = 
	[ ((altMask .|. controlMask,	 xK_l	), spawn "slock")
	, ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
	, ((modm .|. shiftMask, xK_v      ), selectWorkspace defaultXPConfig)
	, ((modm .|. shiftMask, xK_n      ), renameWorkspace defaultXPConfig)
	]
    	++
	zip (zip (repeat modm) [xK_0]) (map (withNthWorkspace W.greedyView) [9])
	++
	zip (zip (repeat (modm .|. shiftMask)) [xK_0]) (map (withNthWorkspace W.shift) [9])
	++
	zip (zip (repeat modm) [xK_F1..xK_F12]) (map (withNthWorkspace W.greedyView) [10..])
	++
	zip (zip (repeat (modm .|. shiftMask)) [xK_F1..xK_F12]) (map (withNthWorkspace W.shift) [10..])

newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))


-- Layouts
myLayout = tall ||| Full ||| Mirror tall
	where
		tall = Tall 1 (3/100) (1/2)

myStatusBar = "dzen2 -ta l -x 0 -y 0 -w 1280 -h 18 -fn inconsolata-16 -fg #a0a0a0 -bg black -dock"

exampleBar = "conky -c ~/.conky/example | dzen2 -x 1280 -y 0 -w 1280 -h 18 -ta r -fn inconsolata-16 -fg #a0a0a0 -bg black -dock"

--Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-y 1000"] }

--dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap "^fg(#00aaff)^bg(#333333)^i(/home/seth/.conky/corner.xbm)^fg(white)" "^bg()^fg()"
    , ppVisible = wrap "^fg(#00aa00)^i(/home/seth/.conky/corner.xbm)^fg(white)" "^fg()"
    , ppHidden = wrap "^i(/home/seth/.conky/corner.xbm)^fg(white)" "^fg()"
    , ppUrgent = wrap "^fg(#ff0000)" "^fg()"
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("white") "" . wrap "-[ " " ]-"
    , ppLayout = dzenColor ("#a0a0a0") "" .
        (\x -> case x of
        "Full" -> "^fg(#666666)^i(/home/seth/.conky/layout_full.xbm)"
        "Tall" -> "^fg(#666666)^i(/home/seth/.conky/layout_tall.xbm)"
        "Mirror Tall" -> "^fg(#666666)^i(/home/seth/.conky/layout_mirror_tall.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }

myManageHook = composeAll
	[ className =? "XMessage" --> doFloat
	, manageDocks
	]

myTrayer = "trayer --edge top --align left --margin 1152 --SetDockType true --SetPartialStrut true --expand true --width 128 --widthtype pixel --transparent true --alpha 1 --tint 0x111111 --height 8"

-- Main

main = do
	dzen <- spawnPipe myStatusBar
	trayer <- spawnPipe myTrayer
	conkyexample <- spawnPipe exampleBar
	xmonad $ myUrgencyHook $ defaultConfig
		{ manageHook		= myManageHook
		, layoutHook 		= avoidStruts $ myLayout
		, workspaces		= (map show [1..10])
		, logHook		= dynamicLogWithPP $ myDzenPP dzen
		, terminal		= myTerminal
		, focusFollowsMouse	= myFocusFollowsMouse
		, clickJustFocuses	= myClickJustFocuses
		, borderWidth		= myBorderWidth
		, modMask		= myModMask
		, normalBorderColor	= myNormalBorderColor
		, focusedBorderColor	= myFocusedBorderColor
		, keys			= newKeys
      		}

