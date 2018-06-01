{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FaustTrading (CoinInfo, getCurrencies) where

import Lens.Micro.Platform
import Network.Wreq
import qualified Data.Text as T
import Data.Scientific
import Data.Aeson.Lens


data CoinInfo = CoinInfo { _coinshort :: T.Text
                         , _coinname :: T.Text
                         , _coincap :: Scientific
                         , _coinvalue :: Scientific
                         } deriving (Show)
makeLenses ''CoinInfo


getCurrencies :: IO [CoinInfo]
getCurrencies = do
  r <- get "http://127.0.0.66:9999/currencies"
  let coins = r ^.. responseBody.values._String
  mapM getInfo coins
  where
    getInfo :: T.Text -> IO CoinInfo
    getInfo name = do
      r <- get ("http://127.0.0.66:9999/info?coin=" ++ T.unpack name)
      let cname  = r ^.. responseBody.key "name"  ._String
      let cvalue = r ^.. responseBody.key "value" ._Number
      let ccap   = r ^.. responseBody.key "cap"   ._Number
      return CoinInfo { _coinshort = name
                      , _coinname = head cname
                      , _coinvalue = head cvalue
                      , _coincap = head ccap
                      }
