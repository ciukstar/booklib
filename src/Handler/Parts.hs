{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Parts
  ( getPartsR, postPartsR
  , getPartR, postPartR
  , getPartNewR
  , getPartEditR
  ) where

import Data.Maybe (isJust)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, selectOne, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert_, replace
    )

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (HomeR, DataR)
    , DataR (PartsR, PartNewR, PartR, PartEditR)
    , AppMessage
      ( MsgParts, MsgPart, MsgSeries, MsgTitle, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgBookParts, MsgBookPart, MsgName, MsgDescription
      )
    )
    
import Model
    ( msgSuccess
    , PartId, Part(Part, partName, partDescr)
    , EntityField (PartName, PartId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod (YesodPersist(runDB), mopt, textareaField, setTitleI)
import Yesod.Core (Yesod(defaultLayout), getMessageRender)
import Yesod.Core.Handler (getMessages, newIdent, addMessageI, redirect)
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    , fvInput, fvLabel, fvErrors
    )


postPartR :: PartId -> Handler Html
postPartR bid = do

    part <- runDB $ selectOne $ do
        x <- from $ table @Part
        where_ $ x ^. PartId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formPart part
    case fr of
      FormSuccess r -> do
          runDB $ replace bid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PartR bid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/parts/edit")


getPartEditR :: PartId -> Handler Html
getPartEditR bid = do

    part <- runDB $ selectOne $ do
        x <- from $ table @Part
        where_ $ x ^. PartId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formPart part
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/parts/edit")

        
postPartsR :: Handler Html
postPartsR = do

    ((fr,fw),et) <- runFormPost $ formPart Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PartsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/parts/new")        
    


getPartNewR :: Handler Html
getPartNewR = do

    (fw,et) <- generateFormPost $ formPart Nothing
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/parts/new")


formPart :: Maybe (Entity Part) -> Form Part
formPart part extra = do

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (partName . entityVal <$> part)
        
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","active")]
        } (partDescr . entityVal <$> part)
    
    let r = Part <$> nameR <*> descrR
    let w = $(widgetFile "data/parts/form")
    return (r,w)


getPartR :: PartId -> Handler Html
getPartR pid = do

    part <- runDB $ selectOne $ do
        x <- from $ table @Part
        where_ $ x ^. PartId ==. val pid
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBookPart
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/parts/part")


getPartsR :: Handler Html
getPartsR = do

    parts <- runDB $ select $ do
        x <- from $ table @Part
        orderBy [asc (x ^. PartId)]
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBookParts
        idOverlay <- newIdent
        $(widgetFile "data/parts/parts")
