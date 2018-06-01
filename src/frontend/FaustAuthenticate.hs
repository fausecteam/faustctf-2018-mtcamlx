{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FaustAuthenticate (enroll, validate, readCryptoParam, schnorrId, Record(..), CryptoParam(..)) where

import Lens.Micro.Platform
import qualified FaustCrypto as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Crypto.Error as E
import qualified Crypto.Number.Serialize as S
import Crypto.Number.ModArithmetic
import qualified Data.Text as T
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Crypto.Hash as H

import Numeric
import Network.Wreq
import Data.Aeson.Lens
import Data.Aeson
import Data.Scientific
import Data.Maybe
import Debug.Trace
import qualified Data.Map as Map

data Record = NoRecord
            | Record { _c1 :: Integer
                     , _c2 :: Integer
                     , _c3 :: Integer
                     } deriving (Show)
makeLenses ''Record

data Proof = Proof { _z :: Integer
                   , _a :: Integer
                   , _b :: Integer
                   } deriving Show
makeLenses ''Proof

data CryptoParam = CryptoParam { _generator :: Integer
                               , _group :: Integer
                               , _publickey :: Integer
                               , _secretkey :: Integer
                               } deriving (Show)
makeLenses ''CryptoParam


readCryptoParam :: IO CryptoParam
readCryptoParam = do
  ptext <- readFile "public.json"
  ctext <- readFile "secret.json"
  let (group, "")     = head $ readHex $ T.unpack $ ptext ^. key "group"     . _String
  let (generator, "") = head $ readHex $ T.unpack $ ptext ^. key "generator" . _String
  let (publickey, "") = head $ readHex $ T.unpack $ ptext ^. key "publickey" . _String
  let (secretkey, "") = head $ readHex $ T.unpack $ ctext ^. key "elgkey"    . _String
  return CryptoParam { _group = group
                     , _generator = generator
                     , _publickey = publickey
                     , _secretkey = secretkey
                     }



genChallenge :: [Integer] -> H.Digest H.SHA256
genChallenge values =
  let
    folder string value =
      let
        newstring = string ++ (showHex value "" ++ "|")
      in newstring
    string2 = foldl folder "" values
  in
    H.hashWith H.SHA256 $ U.fromString string2


validateProof :: CryptoParam -> Proof -> (Integer, Integer) -> (Integer, Integer) -> Bool
validateProof param proof t' c' =
  let Proof { _z=z, _a=a, _b=b } = proof
      CryptoParam { _generator=generator, _group=group , _publickey=publickey } = param
      (c1, co) = c'
      (t1, to) = t'
      challenge = show $ genChallenge [group, generator, {-publickey,-} a, b, c1, co, t1, to]
      (c, "") = head $ readHex challenge
      supposeda = ((expSafe t1 z group) * (expSafe to c group)) `mod` group
      supposedb = ((expSafe c1 z group) * (expSafe co c group)) `mod` group
  in
    (a `mod` group) == supposeda
    && (b `mod` group) == supposedb



enroll :: CryptoParam -> String -> String -> IO Record
enroll cparam username password = do
  random <- C.randZ
  let CryptoParam { _generator=generator, _group=group , _publickey=publickey, _secretkey=secret } = cparam
  let message = S.os2ip $ U.fromString password
  let v = expSafe message (random * secret) group
  let parse resp field =
        result
        where
          tmp = head $ resp ^.. responseBody.key (T.pack field)._String
          (result, "") = head $ readHex $ T.unpack tmp
  auth <- schnorrId cparam
  let opts = defaults
             & param "t" .~ [T.pack username]
             & param "v" .~ [T.pack $ showHex v ""]
             & header "Authentication" .~ [BS.pack $ map BS.c2w auth]
  response <- getWith opts "http://127.0.0.66:9997/enroll"
  let prec3 = parse response "c3"
  let v' = expSafe message ((random - 1) * secret) group
  let Just invv = inverse v' group
  return Record { _c1 = parse response "c1"
                , _c2 = parse response "c2"
                , _c3 = (prec3 * invv) `mod` group
                }
 

validate :: CryptoParam -> Record -> String -> String -> IO (Maybe [String])
validate cparam record username password =
  let Record { _c1=t1, _c2=t2, _c3=t3 } = record in
    do
      random <- C.randZ
      let CryptoParam { _generator=generator, _group=group , _publickey=publickey, _secretkey=secret } = cparam
      let message = S.os2ip $ U.fromString password
      let v = expSafe message (random * secret) group
      let parseR resp field =
            result
            where
              tmp = head $ resp ^.. responseBody.key (T.pack field)._String
              (result, "") = head $ readHex $ T.unpack tmp
      let parseP resp proof =
            Proof { _z = getProofField resp proof "z"
                  , _a = getProofField resp proof "A"
                  , _b = getProofField resp proof "B"
                  }

            where
              getProofField resp proof field =
                result
                where
                  tmp = head $ resp ^.. responseBody.key (T.pack proof).key (T.pack field)._String
                  (result, "") = head $ readHex $ T.unpack tmp
      auth <- schnorrId cparam
      let opts = defaults
                 & param "t"  .~ [T.pack username]
                 & param "v"  .~ [T.pack $ showHex v ""]
                 & param "t1" .~ [T.pack $ showHex t1 ""]
                 & header "Authentication" .~ [BS.pack $ map BS.c2w auth]
      response <- getWith opts "http://127.0.0.66:9997/validate"
      let params = CryptoParam { _group=group, _generator=2, _publickey=14 , _secretkey=15 }
      let proof1t = validateProof params
                                  (parseP response "proof1")
                                  (t1, t2)
                                  ((parseR response "c1"), (parseR response "c2"))
      let proof2t = validateProof params
                                  (parseP response "proof2")
                                  (t1, ((t3 * (fromJust $ (inverse (expSafe message secret group) group))) `mod` group))
                                  ((parseR response "c1"), (((parseR response "c3") * (fromJust $ inverse v group)) `mod` group))
      case proof1t && proof2t of
        True -> return Nothing
        False -> do
          let bs = head (response ^..responseBody) :: L.ByteString
          return $ Just [ (map BS.w2c $ BS.unpack $ L.toStrict bs)
                        , show record
                        , show cparam
                        ]

schnorrId :: CryptoParam -> IO String
schnorrId param = do
  v <- C.randZ
  let CryptoParam { _generator=generator, _group=group , _secretkey=secretkey } = param
  let publickey = expSafe generator secretkey group
  let g2v = expSafe generator v group
  let ch = genChallenge [generator, group, g2v, publickey]
  let (c, "") = head $ readHex $ show ch
  let r = (v - secretkey * c) `mod` (group - 1)

  return $ map BS.w2c $ BS.unpack $ L.toStrict $ encode $ Map.fromList [('r', (showHex r "")), ('V', (showHex g2v ""))]

