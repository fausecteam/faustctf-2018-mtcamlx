{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Main
import Brick.Forms
import qualified Brick.Widgets.Dialog as D
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Text as T
import Control.Monad.IO.Class

import Network.Wreq
import Data.Aeson.Lens
import Data.Scientific
import Data.Time
import Text.Wrap

import FaustAuthenticate
import FaustPersist
import FaustTrading
import FaustMessage
import FaustEvent

makeLenses ''CoinInfo
makeLenses ''MessageInfo
makeLenses ''State
makeLenses ''LoginInformation
makeLenses ''Account
makeLenses ''NewMessageInformation


--------
-- Forms

mkLoginForm :: LoginInformation -> Form LoginInformation e Name
mkLoginForm =
  let label s w = padBottom (Pad 1) $
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in
    newForm [ label "Username" @@= editTextField name NameField (Just 1)
            , label "Password" @@= editPasswordField password PasswordField
            ]


mkNewMessageForm :: NewMessageInformation -> Form NewMessageInformation e Name
mkNewMessageForm =
  let label s w = padBottom (Pad 1) $
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in
    newForm [ label "Recipient" @@= editTextField recipient RecipientField (Just 1)
            , label "Text"      @@= editTextField text TextField (Just 10)
            ]


----------
-- Drawing
drawUI :: State -> [Widget Name]
drawUI s =
  (drawErrorUI s) ++ (drawHelpUI s) ++ (drawLoginUI s) ++ (drawNewMessageUI s) ++ (drawMainUI s)


drawErrorUI :: State -> [Widget Name]
drawErrorUI s =
  case s ^. errorstr of
    [] -> []
    (x:xs) ->
      [diag]
      where
        errormsgw = strWrapWith (defaultWrapSettings {breakLongWords = True}) x
        diag = D.renderDialog (s^.dialog) $ padAll 2 $ (str "Login Failed" <=> errormsgw)


drawNewMessageUI :: State -> [Widget Name]
drawNewMessageUI s =
  case view mode s of
    MessageMode ->
      [ centerLayer $
        borderWithLabel (str "New Message") $
        hLimit 60 $
        vLimit 15 $
        withBorderStyle unicode $
        center centerui ]
      where
        formui = renderForm $ view messageform s
        centerui = str "To send press ^w" <=> (padBottom (Pad 1) $ str "To cancel press ^e") <=> formui
    _ -> []


drawHelpUI :: State -> [Widget Name]
drawHelpUI s =
  case view mode s of
    HelpMode ->
      [ centerLayer $
        borderWithLabel (str "Help") $
        hLimit 60 $
        vLimit 15 $
        withBorderStyle unicode $
        center centerui ]
      where
        shortcuts = str "?" <=> str "^r" <=> str "^d" <=> str "m" <=> str "[ESC]" <=> str "r"
        expansions = str "Help" <=> str "Register" <=> str "Debug" <=> str "New Message"
          <=> str "Quit" <=> str "Refresh current values"
        centerui = shortcuts <+> padLeft (Pad 5) expansions
    _ -> []


drawLoginUI :: State -> [Widget Name]
drawLoginUI s =
  case view (account . username) s of
    Nothing ->
      [ centerLayer $
        borderWithLabel (str (case view mode s of
                                RegisterMode -> "Register"
                                _ -> "Login")) $
        hLimit 40 $
        vLimit 7 $
        withBorderStyle unicode $
        center centerui ]
      where
        baseui = renderForm $ view loginform s
        centerui = case view debugmode s of
          True -> baseui <=> str "DEBUGMODE"
          False -> padBottom (Pad 1) baseui
    Just _ -> []


drawMessageUI :: State -> Widget Name
drawMessageUI s =
  (padBottom (Pad 1) $ str "Messages") <=> hLimit 80 messagewidget
  where
    messagelist = view messages s
    messagewidget = case messagelist of
      [] -> str "You have no messages"
      _ ->
        foldl1 (<=>) widgetlist
        where
          formatmessage mess =
            (meta <+> vLimit 2 vBorder <+> middle) <=> hBorder
            where
              middle = padLeft (Pad 2) $ str (T.unpack $ view messagecontent mess)
              meta = (str (T.unpack $ view messagesource mess))
                   <=> str (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                            $ view messagedate mess)
          widgetlist = map formatmessage messagelist



drawPriceUI :: State -> Widget Name
drawPriceUI s =
  str "Price UI" <=> hBorder <=> str " " <=>
  (hLimit 6 shortPlane <+> padLeft (Pad 3) namePlane
   <+> padLeft (Pad 3) valuePlane  <+> padLeft (Pad 3) capPlane)
  where
    currencylist = view currencies s
    shortPlane   = str "Short"     <=> hBorder
      <=> (foldl1 (<=>) $ map (\ci -> str $ T.unpack $ view coinshort ci)
                              currencylist)
    namePlane    = str "Name"      <=> hBorder
      <=> (foldl1 (<=>) $ map (\ci -> str $ T.unpack $ view coinname  ci)
                              currencylist)
    valuePlane   = str "Value"     <=> hBorder
      <=> (foldl1 (<=>) $ map (\ci -> str $ show $ view coinvalue     ci)
                              currencylist)
    capPlane     = str "Marketcap" <=> hBorder
      <=> (foldl1 (<=>) $ map (\ci -> str $ show $ view coincap       ci)
                              currencylist)



drawAccountUI :: State -> Widget Name
drawAccountUI s =
  vLimit 7 $ vCenter $
  case view (account . username) s of
    Nothing ->
      (padBottom (Pad 2) $ str "You are not logged in")
      <=> (padBottom (Pad 2) $ str "Log in for more information")
    Just name ->
      (padBottom (Pad 2) $ str ("Welcome back " ++ name))
      <=> (padBottom (Pad 2) $ str "You can now leverage the full power of MtCamlX")
      <=> str ("Your account balance is 42 Tether (IRR)")


drawMainUI s = [ui1]
  where
    ui1 =
      withBorderStyle unicode $
      borderWithLabel (str "MtCamlX") $
      ((drawAccountUI s)
        <=> hBorder
        <=> (drawMessageUI s))
      <+> vBorder
      <+> hCenter (drawPriceUI s)


theMap :: AttrMap
theMap = attrMap V.defAttr [ (E.editAttr, V.white `on` V.black)
                           , (E.editFocusedAttr, V.black `on` V.yellow)
                           , (D.buttonSelectedAttr, bg V.green)
                           ]

-- ui :: Widget ()
-- ui =

myApp = App { appDraw = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent = setupEvent
            , appAttrMap = const theMap
            }


main :: IO State
main = do
  param <- readCryptoParam
  Brick.defaultMain myApp
    State { _account = Account { _username = Nothing }
          , _debugmode = False
          , _errorstr = []
          , _mode = LoginMode
          , _dialog = D.dialog (Just "Error") (Just (0, [ ("OK", DialogResultOK)
                                                        , ("CANCEL", DialogResultCancel)
                                                        ])) 400
          , _loginform = mkLoginForm
            LoginFormState { _name = ""
                           , _password = ""}
          , _messageform = mkNewMessageForm
            NewMessageInformation { _recipient = ""
                                  , _text = ""}
          , _currencies = []
          , _messages = []
          , _cryptoparam = param
           }
