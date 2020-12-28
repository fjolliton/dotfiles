-- Installation
-- ------------
--
-- For Debian:
-- apt-get install cabal-install
-- apt-get install libxinerama-dev # for Xinerama support
-- apt-get install libxft-dev # for X11-xft support
-- cabal update
-- cabal install xmonad xmonad-contrib
-- cabal install xmobar -fwith_xft -fwith_datezone
--
-- If something goes wrong, try to do cleanup in ~/.ghc and ~/.cabal.
--
-- If XMonad is not found, try something like:
-- $ mkdir -p ~/.ghc/x86_64-linux-8.8.3/
-- $ ln -s ~/.cabal/store/ghc-8.8.3/package.db ~/.ghc/x86_64-linux-8.8.3/package.conf.d

import Data.Bits
import qualified Data.List as L
import qualified Data.Map as M

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Util.Run
import XMonad.Actions.SwapWorkspaces

import qualified XMonad.StackSet as W

-- https://hackage.haskell.org/package/xmonad-contrib-0.14/docs/
-- Other interesting layout: MagicFocus, Master, Renamed, ResizableTile
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.IfMax
import XMonad.Layout.Renamed

toggleStrutsKey XConfig { modMask = modMask } = (modMask, xK_b)

myPP = xmobarPP {
    ppTitle = xmobarColor "#aaaaaa" "" -- . wrap "<fn=1><fc=#aaaaaa,#000000>\xe0b2</fc></fn> " " <fn=1><fc=#aaaaaa,#000000>\xe0b0</fc></fn>"
  , ppSep = " <fc=#080808><fn=1>│</fn></fc> "
  , ppCurrent = \w -> xmobarColor "#ee3" "" ("[" ++ w ++ "]")
  , ppVisible = \w -> xmobarColor "#7f7" "" ("(" ++ w ++ ")")
  , ppHidden = \w -> xmobarColor "#888" "" w
  , ppHiddenNoWindows = \ws -> xmobarColor "#484848" "" ws
  , ppOrder = \(ws:l:t:_) -> [ws, (wrap "<fc=#888888>" "</fc>" l), t]
}

myExtraWorkspaces = [(xK_0, "0")]

myWorkspaces = [(xK_1, "1"), (xK_2, "2"), (xK_3, "3"), (xK_4, "4"), (xK_5, "5"), (xK_6, "6"), (xK_7, "7"), (xK_8, "8"), (xK_9, "9")] ++ myExtraWorkspaces

-- Replace all keys 'ks' with modSrc to modDst
changeModifiers :: M.Map (KeyMask, KeySym) (X ()) -> [KeySym] -> KeyMask -> KeyMask -> M.Map (KeyMask, KeySym) (X ())
changeModifiers keys ks modSrc modDst = M.fromList $ map change $ M.toList keys
  where change :: ((KeyMask, KeySym), (X ())) -> ((KeyMask, KeySym), (X ()))
        change item@((mod, key), action) = if key `L.elem` ks && mod .&. modSrc /= 0
                                           then (((mod .&. complement modSrc) .|. modDst, key), action)
                                           else item

-- Use the left Windows key to change workspace instead of the Alt key.
alterKeys keys = changeModifiers keys (xK_Tab:[xK_1 .. xK_9]) mod1Mask mod2Mask

-- FIXME: Should be desktop specific
leftMonitor = "DVI-I-1" -- "DP1"
rightMonitor = "DVI-D-0" -- "VGA1"

myKeys XConfig { modMask = modm } = M.fromList $
    [
      -- Menu key (mod3)
      ((mod3Mask, xK_s), safeSpawn "firefox" ["-ProfileManager", "-no-remote"])
    , ((mod3Mask, xK_x), safeSpawn "emacs" [])
    , ((mod3Mask, xK_m), safeSpawn "urxvt" [])
    , ((mod3Mask, xK_w), safeSpawn "urxvt" ["-fn", "xft:Inconsolata:pixelsize=48,xft:Fixed"])
    , ((mod3Mask, xK_t), safeSpawn "alacritty" [])
    , ((mod3Mask, xK_j), safeSpawn "urxvt" ["-e", "node-repl"])
    , ((mod3Mask, xK_r), safeSpawn "urxvt" ["-e", "./js/node_modules/.bin/rtop"])
    , ((mod3Mask, xK_g), safeSpawn "urxvt" ["-e", "ghci"])
    , ((mod3Mask, xK_l), safeSpawn "urxvt" ["-e", "clisp"])
    , ((mod3Mask .|. shiftMask, xK_l), safeSpawn "urxvt" ["-e", "lua5.3"])
    , ((mod3Mask, xK_p), safeSpawn "urxvt" ["-e", "/home/fred/py3/bin/python3"])
    , ((mod3Mask .|. shiftMask, xK_p), safeSpawn "urxvt" ["-e", "/home/fred/py2/bin/python2"])
    , ((mod3Mask, xK_slash), safeSpawn "killall" ["-CONT", "blender27"])
    , ((mod3Mask .|. shiftMask, xK_slash), safeSpawn "killall" ["-STOP", "blender27"])

    , ((mod2Mask .|. shiftMask, xK_Up), windows W.swapUp)
    , ((mod2Mask .|. shiftMask, xK_Down), windows W.swapDown)

      -- Left Alt key (mod1)
    , ((mod1Mask .|. controlMask, xK_Left), safeSpawn "screencontrol" ["toggle-right"])
    , ((mod1Mask .|. controlMask, xK_Right), safeSpawn "screencontrol" ["toggle-left"])
    , ((mod1Mask .|. controlMask, xK_Down), safeSpawn "screencontrol" ["toggle-scale"])
    , ((mod1Mask .|. controlMask, xK_Up), safeSpawn "screencontrol" ["reset"])

      -- Left Windows key (mod2)
    , ((mod2Mask, xK_F12), safeSpawn "screenshot" [])
    , ((mod2Mask .|. shiftMask, xK_F12), safeSpawn "screenshot-edit-publish" [])
    ] ++ [((mod2Mask, key), (windows $ W.greedyView ws)) | (key,ws) <- myExtraWorkspaces
    ] ++ [((mod2Mask .|. shiftMask, key), (windows $ W.shift ws)) | (key,ws) <- myExtraWorkspaces
    ] ++ [((mod2Mask .|. controlMask, k), windows $ swapWithCurrent i)
          | (k, i) <- myWorkspaces]

windowGap :: Int
windowGap = 2

uniformBorder :: Integer -> Border
uniformBorder i = Border i i i i

-- FIXME: Put the styling in dynamic log ppOrder
myLayout = renameLayout "Full" Full
           ||| (layoutWrapper "Tall" $ Tall 1 (3/100) (1/2))
           -- ||| (layoutWrapper "Mirror Tall" $ Mirror (Tall 1 (3/100) (1/2)))
           -- ||| (layoutWrapper "Grid" $ Grid)
  where layoutWrapper name = renameLayout name
                             . IfMax 1 Full
                             . spacingRaw True (uniformBorder 0) True (uniformBorder $ toInteger windowGap) True
                             . gaps [(U, -windowGap), (D, -windowGap), (R, -windowGap), (L, -windowGap)]
        renameLayout name = renamed [Replace name]

myConfig = defaultConfig {
    terminal = "urxvt"
  , borderWidth = 0
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#000000"
  , focusFollowsMouse = False
  , clickJustFocuses = False
  , keys = \conf -> myKeys conf `M.union` alterKeys (keys defaultConfig conf)
  , startupHook = setWMName "LG3D"
  , workspaces = map snd myWorkspaces
  , layoutHook = myLayout
}

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig
