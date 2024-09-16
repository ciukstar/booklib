{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Topics
  ( getTopicsR, postTopicsR
  , getTopicR, postTopicR
  , getTopicNewR
  , getTopicEditR
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
    , Route (DataR)
    , DataR (TopicsR, TopicNewR, TopicR, TopicEditR)
    , AppMessage
      ( MsgTopics, MsgTopic, MsgTitle, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgTopics, MsgTopic, MsgName, MsgDescription
      )
    )
    
import Model
    ( msgSuccess
    , TopicId, Topic(Topic, topicName, topicDescr)
    , EntityField (TopicName, TopicId)
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


postTopicR :: TopicId -> Handler Html
postTopicR bid = do

    topic <- runDB $ selectOne $ do
        x <- from $ table @Topic
        where_ $ x ^. TopicId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formTopic topic
    case fr of
      FormSuccess r -> do
          runDB $ replace bid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ TopicR bid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/topics/edit")


getTopicEditR :: TopicId -> Handler Html
getTopicEditR bid = do

    topic <- runDB $ selectOne $ do
        x <- from $ table @Topic
        where_ $ x ^. TopicId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formTopic topic
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/topics/edit")

        
postTopicsR :: Handler Html
postTopicsR = do

    ((fr,fw),et) <- runFormPost $ formTopic Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR TopicsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/topics/new")        
    


getTopicNewR :: Handler Html
getTopicNewR = do

    (fw,et) <- generateFormPost $ formTopic Nothing
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/topics/new")


formTopic :: Maybe (Entity Topic) -> Form Topic
formTopic part extra = do

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (topicName . entityVal <$> part)
        
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","active")]
        } (topicDescr . entityVal <$> part)
    
    let r = Topic <$> nameR <*> descrR
    let w = $(widgetFile "data/topics/form")
    return (r,w)


getTopicR :: TopicId -> Handler Html
getTopicR tid = do

    topic <- runDB $ selectOne $ do
        x <- from $ table @Topic
        where_ $ x ^. TopicId ==. val tid
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTopic
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/topics/topic")


getTopicsR :: Handler Html
getTopicsR = do

    topics <- runDB $ select $ do
        x <- from $ table @Topic
        orderBy [asc (x ^. TopicName)]
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTopics
        idOverlay <- newIdent
        $(widgetFile "data/topics/topics")
