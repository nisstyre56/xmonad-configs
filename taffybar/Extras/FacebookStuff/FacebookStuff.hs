{-# LANGUAGE OverloadedStrings #-}

module FacebookStuff where

import Network.Xmpp

hostname = "chat.facebook.com"
username = "wes.kerfoot@chat.facebook.com"
password = ""

fbSession = session hostname (simpleAuth username password) def
