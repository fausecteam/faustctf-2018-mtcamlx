{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FaustMessage (getMessages, sendMessage, MessageInfo(..)) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import Data.Time
import Lens.Micro.Platform
import Data.Aeson.Lens
import Network.Wreq


data MessageInfo = MessageInfo { _messagesource :: T.Text
                               , _messagecontent :: T.Text
                               , _messagedate :: UTCTime
                               } deriving (Show)
makeLenses ''MessageInfo


sendMessage :: String -> String -> String -> String -> IO ()
sendMessage auth from to text = do
  let opts = defaults
             & param "message" .~ [T.pack text]
             & param "from"    .~ [T.pack from]
             & param "to"      .~ [T.pack to]
             & header "Authentication" .~ [BS.pack $ map BS.c2w auth]
  _ <- getWith opts "http://127.0.0.66:9998/post"
  return ()


getMessages :: String -> String -> IO [MessageInfo]
getMessages auth name = do
  let opts = defaults
             & param "user" .~ [T.pack name]
             & header "Authentication" .~ [BS.pack $ map BS.c2w auth]
  r <- getWith opts "http://127.0.0.66:9998/get"
  let messages = r ^.. responseBody.values
  return $ map parseMessages messages
  where
    parseMessages message =
            MessageInfo { _messagesource = msource
                        , _messagecontent = mcontent
                        , _messagedate = mdate
                        }
      where
        msource     = message ^. key "from"    ._String
        mcontent    = message ^. key "message" ._String
        mdatestring = message ^. key "time"    . _String
        Just mdate = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
          $ T.unpack mdatestring :: Maybe UTCTime
