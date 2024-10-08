{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  ) where


import Foundation
    ( Handler
    , widgetSnackbar, widgetTopbar
    , AppMessage
      ( MsgAppName, MsgHome
      )
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod
    ( getMessageRender
    )
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages
    )
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome
        idOverlay <- newIdent
        $(widgetFile "homepage")
