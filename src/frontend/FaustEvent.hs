{-# LANGUAGE TemplateHaskell #-}

module FaustEvent ( handleEvent
                  , setupEvent
                  , State(..)
                  , Account(..)
                  , LoginInformation(..)
                  , Name(..)
                  , Event
                  , UIMode(..)
                  , NewMessageInformation(..)
                  , DialogResult(..)
                  ) where

import Brick.Forms
import Brick.Widgets.Dialog
import Brick.Types
import Brick.Main
import qualified Data.Text as T
import Lens.Micro.Platform
import qualified Graphics.Vty as V
import Control.Monad.IO.Class

import FaustAuthenticate
import FaustTrading
import FaustMessage
import FaustPersist

data DialogResult = DialogResultOK
                  | DialogResultCancel

data Name = NameField
          | PasswordField
          | RecipientField
          | TextField
          deriving (Eq, Ord, Show)
data Event = Event


data Account = Account { _username :: Maybe String
                       } deriving (Show)

data LoginInformation = LoginFormState { _name :: T.Text
                                       , _password :: T.Text
                                       } deriving (Show)

data NewMessageInformation = NewMessageInformation { _recipient :: T.Text
                                                   , _text :: T.Text
                                                   } deriving (Show)

data UIMode = RegisterMode
            | MessageMode
            | HelpMode
            | NormalMode
            | LoginMode
            | ErrorMode
            deriving (Show, Eq)

data State = State { _account :: Account
                   , _debugmode :: Bool
                   , _errorstr :: [String]
                   , _mode :: UIMode
                   , _dialog :: Dialog DialogResult
                   , _loginform :: Form LoginInformation Event Name
                   , _messageform :: Form NewMessageInformation Event Name
                   , _currencies :: [CoinInfo]
                   , _messages :: [MessageInfo]
                   , _cryptoparam :: CryptoParam
                   }

makeLenses ''State
makeLenses ''Account
makeLenses ''LoginInformation
makeLenses ''NewMessageInformation

returnToNormal :: State -> State
returnToNormal s =
  case s^.errorstr of
    [] ->
      case view ( account . username ) s of
        Nothing -> set mode LoginMode s
        _ -> set mode NormalMode s
    _ -> set mode ErrorMode s


handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey V.KEsc []))   = halt s
handleEvent s (VtyEvent (V.EvKey V.KEnter []))
  | view mode s == ErrorMode = do
      continue $ returnToNormal $ over errorstr tail s
  | view mode s == RegisterMode = do
      let formstate = (formState $ s^.loginform)
      let un = T.unpack (formstate^.name)
      let pw = T.unpack (formstate^.password)
      rc <- liftIO $ enroll (view cryptoparam s) un pw
      liftIO $ saveRecord un rc
      continue $ returnToNormal s
  | view mode s == LoginMode = do
      let formstate = (formState $ s^.loginform)
      let un = T.unpack (formstate^.name)
      let pw = T.unpack (formstate^.password)
      rc <- liftIO $ getRecord un
      case rc of
        Nothing -> halt s
        Just record -> do
          validateResult <- liftIO $ validate (view cryptoparam s) record un pw
          case validateResult of
            Nothing -> continue $ set mode NormalMode $ set (account . username) (Just $ T.unpack $ formstate^.name) s
            Just errormsg -> continue $ set mode ErrorMode $ over errorstr (\x -> errormsg ++ x) s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'r') []))
  | view mode s == NormalMode = do
      let Just name = view (account . username) s
      entries <- liftIO getCurrencies
      auth <- liftIO $ schnorrId (view cryptoparam s)
      newmessages <- liftIO $ getMessages auth name
      let s' = set messages newmessages s
      continue $ set currencies entries s'
handleEvent s (VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl]))
  | view mode s == MessageMode = do
      let Just name = view (account . username) s
      let formstate = (formState $ s^.messageform) :: NewMessageInformation
      let therecipient = T.unpack (formstate^.recipient)
      let thetext = T.unpack (formstate^.text)
      auth <- liftIO $ schnorrId (view cryptoparam s)
      liftIO $ sendMessage auth name therecipient thetext
      continue $ returnToNormal s
handleEvent s (VtyEvent (V.EvKey (V.KChar '?') []))
  | view mode s == HelpMode = do continue $ returnToNormal s
  | view mode s == NormalMode = do continue $ set mode HelpMode s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'm') []))
  | view mode s == NormalMode = do continue $ set mode MessageMode s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl]))
  | view mode s == LoginMode = do continue $ set mode RegisterMode s
  | view mode s == RegisterMode = do continue $ returnToNormal s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl]))
  | view mode s == MessageMode = do continue $ returnToNormal s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = do
  continue $ over debugmode not s
handleEvent s (VtyEvent ev)
  | view mode s == ErrorMode =
    continue =<< handleEventLensed s (FaustEvent.dialog) handleDialogEvent ev
handleEvent s e =
  case view mode s of
    MessageMode ->
      continue =<< handleEventLensed s messageform handleFormEvent e
    LoginMode ->
      continue =<< handleEventLensed s loginform handleFormEvent e
    RegisterMode ->
      continue =<< handleEventLensed s loginform handleFormEvent e
    _ -> continue s


setupEvent :: State -> EventM Name State
setupEvent s = do
  liftIO setupDatabase
  entries <- liftIO getCurrencies
  return $ set currencies entries s
