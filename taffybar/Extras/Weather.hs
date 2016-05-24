{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Extras.Weather where

import Data.Text (unpack, strip)
import System.Taffybar.Widgets.PollingLabel
import Network.HTTP.Conduit
import Text.HTML.DOM (parseLBS)
import Control.Applicative
import Graphics.UI.Gtk
import Control.Monad
import Control.Exception
import Data.Maybe
import Text.XML.Cursor (attributeIs,
                        (&/),
                        (&|),
                        element,
                        fromDocument,
                        attribute,
                        ($//),
                        ($/),
                        content,
                        descendant,
                        ancestor,
                        following,
                        child,
                        check)

docCursor = fromDocument <$>
            parseLBS <$>
            simpleHttp "http://geomedia.mcmaster.ca/muws/index.php"

tempSelector = (attributeIs "class" "style4") &/ content

temp = do
  doc <- docCursor
  let tempText = doc $// tempSelector
  return $ unpack $ strip $ head tempText

makeOutPut url title =
  "<span foreground='white'><a href=\"" ++ url ++ "\"><span foreground='white'>"++ title ++"</span></a>| </span>"

weatherWidget :: IO Widget
weatherWidget = do
  l <- pollingLabelNew (makeOutPut "" "Nothing") (16*60) temp
  widgetShowAll l
  return (toWidget l)
