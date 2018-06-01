module FaustCrypto where

import qualified Data.ByteString as BS

import Crypto.Number.ModArithmetic
import Crypto.Number.Serialize
import Crypto.Random


randZ :: IO Integer
randZ = do
  generator <- drgNew :: IO ChaChaDRG
  b <- getRandomBytes 16 :: IO BS.ByteString
  return $ os2ip b

randG :: Integer -> Integer -> IO Integer
randG generator group = do
  exponent <- randZ
  return $ expSafe generator exponent group
