{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Extras.Weather where

import Network.MPD
import Data.Text (unpack, strip)
import System.Taffybar.Widgets.PollingLabel
import Network.HTTP.Conduit
import Text.HTML.DOM (parseLBS)
import Control.Applicative
import Graphics.UI.Gtk
import Control.Monad
import Control.Exception
import Data.Maybe
import System.Taffybar.Widgets.Util

