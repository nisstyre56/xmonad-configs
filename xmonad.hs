{-# LANGUAGE ImplicitParams #-}
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh) -- for taffypager
import XMonad.Layout.NoBorders
import Control.Monad
import XMonad.Layout.SimpleFloat
import XMonad.Layout.PerWorkspace (onWorkspace)
import qualified Data.Map as M
import XMonad.Actions.CycleWS
import Control.Applicative
import Control.Monad
import DBus
import DBus.Client

import XMonad.Hooks.FadeWindows

customWorkspaces = take 9 $ map return ['A'..'I']

(==>) :: (?modm :: KeyMask, MonadIO m) => KeySym -> m () -> ((KeyMask, KeySym), m ())

key ==> name = ((?modm, key), name)

fadehook = composeAll [
  isUnfocused --> opaque,
  opaque
      ]

start client = xmonad $ ewmh
        defaultConfig
            { logHook = (fadeWindowsLogHook fadehook)
            , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
            , handleEventHook = fadeWindowsEventHook
            , terminal = "urxvt -e tmux"
            , workspaces = customWorkspaces
            , manageHook = manageDocks
            , startupHook = spawn "startxmonad" >> spawn "xbacklight -set 80" >> spawn "/usr/bin/xscreensaver -no-splash &" >> spawn "exec --no-startup-id nm-applet"
            , keys = \c -> mykeys c `M.union` keys defaultConfig c
            } where
              mykeys (XConfig {modMask = modm}) =
                let ?modm = modm in M.fromList [
                      xK_f ==> spawn "firefox",
                      xK_g ==> spawn "gvim",
                      xK_i ==> spawn "pidgin",
                      xK_u ==> spawn "amixer sset Master 2%+",
                      xK_d ==> spawn "amixer sset Master 2%-",
                      xK_F8 ==> spawn "xbacklight -dec 5",
                      xK_F9 ==> spawn "xbacklight -inc 5",
                      xK_grave ==> spawn "mpc toggle",
                      xK_F5 ==> spawn "mpc prev",
                      xK_F6 ==> spawn "mpc next",
                      xK_F4 ==> spawn "mpc toggle",
                      xK_Delete ==> spawn "xscreensaver-command -lock",
                      xK_bracketleft ==> prevWS,
                      xK_bracketright ==> nextWS,
                      xK_r ==> spawn "drracket",
                      xK_s ==> spawn "scrot"]

main = join $ start <$> connectSession
