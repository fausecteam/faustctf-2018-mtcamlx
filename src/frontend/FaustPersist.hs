{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module FaustPersist (setupDatabase, saveRecord, getRecord) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import FaustAuthenticate (Record(..))
import Numeric (showHex, readHex)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    NameKey name
    t1 String
    t2 String
    t3 String
    deriving Show
|]

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id

setupDatabase :: IO ()
setupDatabase = runSqlite "persistent.sqlite" $ do
    runMigration migrateAll


saveRecord :: String -> Record -> IO ()
saveRecord username record = runSqlite "persistent.sqlite" . asSqlBackendReader $ do
  let Record { _c1=t1, _c2=t2, _c3=t3 } = record
  insert $ Person username (showHex t1 "") (showHex t2 "") (showHex t3 "")
  return ()


getRecord :: String -> IO (Maybe Record)
getRecord username =
  runSqlite "persistent.sqlite" . asSqlBackendReader $ do
  personRecord <-
    getBy $ NameKey username

  case personRecord of
    Just precord -> do
      let Person {personT1=st1, personT2=st2, personT3=st3} = entityVal precord
      let (t1, "") = head $ readHex st1
      let (t2, "") = head $ readHex st2
      let (t3, "") = head $ readHex st3

      return $ Just Record {_c1=t1, _c2=t2, _c3=t3 }
    Nothing -> do
      return Nothing
