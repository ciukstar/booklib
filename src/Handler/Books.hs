{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Books
  ( getBooksR, postBooksR
  , getBookR, postBookR
  , getBookNewR
  , getBookEditR
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
    , DataR (BooksR, BookNewR, BookR, BookEditR)
    , AppMessage
      ( MsgBooks, MsgBook, MsgSeries, MsgTitle, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited
      )
    )
    
import Model
    ( BookId, Book(Book, bookSeries, bookTitle)
    , EntityField (BookTitle, BookId), msgSuccess
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod (YesodPersist(runDB))
import Yesod.Core (Yesod(defaultLayout), getMessageRender)
import Yesod.Core.Handler (getMessages, newIdent, addMessageI, redirect)
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    , fvInput, fvLabel, fvErrors
    )


postBookR :: BookId -> Handler Html
postBookR bid = do

    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        return x

    ((fr,fw),et) <- runFormPost $ formBook book
    case fr of
      FormSuccess r -> do
          runDB $ replace bid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ BookR bid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/books/edit")


getBookEditR :: BookId -> Handler Html
getBookEditR bid = do

    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        return x

    (fw,et) <- generateFormPost $ formBook book
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/books/edit")

        
postBooksR :: Handler Html
postBooksR = do

    ((fr,fw),et) <- runFormPost $ formBook Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR BooksR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/books/new")        
    


getBookNewR :: Handler Html
getBookNewR = do

    (fw,et) <- generateFormPost $ formBook Nothing
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/books/new")


formBook :: Maybe (Entity Book) -> Form Book
formBook book extra = do

    (seriesR,seriesV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgSeries
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (bookSeries . entityVal <$> book)
        
    (titleR,titleV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (bookTitle . entityVal <$> book)
    
    let r = Book <$> seriesR <*> titleR
    let w = $(widgetFile "data/books/form")
    return (r,w)


getBookR :: BookId -> Handler Html
getBookR bid = do

    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/books/book")


getBooksR :: Handler Html
getBooksR = do

    books <- runDB $ select $ do
        x <- from $ table @Book
        orderBy [asc (x ^. BookTitle)]
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/books/books")
