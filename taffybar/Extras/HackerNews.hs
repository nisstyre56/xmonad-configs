{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Extras.HackerNews where

import Data.Text (unpack)
import System.Taffybar.Widgets.PollingLabel
import Control.Concurrent.Async
import Network.HTTP.Conduit
import Text.HTML.DOM (parseLBS)
import Control.Applicative
import Graphics.UI.Gtk
import Control.Monad
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import Control.Exception
import Data.Maybe
import System.Taffybar.Widgets.Util
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

choose [] = return ("", "Nothing")
choose xs = do
  x <- runRVar (choice xs) DevRandom
  return x

titleSelector = (attributeIs "class" "title") &/ element "a" &/ content
urlSelector = join . ((attributeIs "class" "title") &/ (element "a") &| (attribute "href"))

docCursor = fromDocument <$>
            parseLBS <$>
            simpleHttp "http://news.ycombinator.com/newest"

links cursor = cursor $// urlSelector

titles cursor = cursor $// titleSelector

submissions cursor =
  let subs = zip (links cursor) (titles cursor)
    in filter check subs where
      check (_,title) = title /= "More" &&
                        title /= "scribd"

--newTitle = makeOutPut <$> (join $ (unpack <$>) <$> choose <$> links <$> docCursor)

newTitle :: IO String
newTitle = do
  doc <- docCursor
  (url, title) <- choose $ submissions doc
  let url' = unpack url
  let title' = unpack title
  return $ makeOutPut url' title'


handleReq :: HttpException -> IO (Maybe String)
handleReq _ = return (Just "")

tryTitle :: IO String
tryTitle = fromJust <$> catch (return <$> newTitle) handleReq

makeOutPut url title =
  "<span foreground='white'><a href=\"" ++ url ++ "\"><span foreground='white'>"++ title ++"</span></a>| </span>"

hnWidget :: IO Widget
hnWidget = do
  l <- pollingLabelNew (makeOutPut "" "Nothing") 60 tryTitle
  widgetShowAll l
  return (toWidget l)
