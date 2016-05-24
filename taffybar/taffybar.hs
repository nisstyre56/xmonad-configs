{-# LANGUAGE ImplicitParams #-}

import System.Taffybar
import Extras.Weather
import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
    ( notifyAreaNew, defaultNotificationConfig )
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.TaffyPager

import System.Information.Memory
import System.Information.CPU
import System.Information.Battery
import System.Posix.Process (getProcessID)
import System.Directory (doesFileExist)
import System.Information.X11DesktopInfo
import Control.Concurrent

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Applicative
import Data.Maybe
import Extras.Weather
import XMonad.Core
import XMonad.Util.Run
import Control.Concurrent (threadDelay)

batCallback context = do
  batinfo <- case context of { (Just c) -> getBatteryInfo c ; Nothing -> return Nothing }
  return $ maybe 1 ((/ 100) . batteryPercentage) batinfo

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

killAndSpawn :: String -> IO ()
killAndSpawn prog = do
  spawn $ "kill -9 $(pgrep -f " ++ prog ++ ")"
  threadDelay $ 1000000 `div` 2
  spawn prog

main = do
  dispCount <- atomically $ newTVar 0
  context <- batteryContextNew
  let memCfg = defaultGraphConfig {
        graphDataColors = [(1, 0, 0, 1)],
        graphLabel = Just "mem"
      }
      cpuCfg = defaultGraphConfig {
        graphDataColors = [ (0, 1, 0, 1)
                            , (1, 0, 1, 0.5) ],
        graphLabel = Just "cpu"
      }
      {-batCfg = defaultBarConfig undefined {
        barBorderColor = (0, 0, 0),
        barBackgroundColor = \x -> (255, 255, 255),
        barColor = \x -> (128, 128, 128),
        barPadding = 3,
        barWidth = 50,
        barDirection = VERTICAL
      }-}
      batCfg = defaultBarConfig (\x ->
          if x >= 0.20 then
            if x < 0.97
             then (0, 255, 0)
            else
              (255, 220, 20)
          else (255, 0, 0))
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %I:%M</span>" 1
      log = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 0.5 memCallback
      bat = pollingBarNew batCfg 1 (batCallback context)
      tray = systrayNew
  pid <- getProcessID
  writeFile "/home/wes/.cache/taffybar.pid" (show pid)
  killAndSpawn "nm-applet"

  defaultTaffybar defaultTaffybarConfig { startWidgets = [log, tray, note, clock]
                                        , endWidgets = [weatherWidget, bat, cpu]
                                        }
