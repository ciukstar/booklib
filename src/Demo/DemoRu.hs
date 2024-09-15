{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Demo.DemoRu (fillDemoRu) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.Time.Clock (getCurrentTime, addUTCTime)

import Database.Persist (PersistStoreWrite (insert, insert_, insertKey))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( Role(Role)
    , User (User, userEmail, userPassword, userRole), Book (Book, bookSeries, bookTitle), Part (Part, partName, partDescr)
    )
    
import System.Random (randomRIO)

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => ReaderT SqlBackend m ()
fillDemoRu = do

    now <- liftIO getCurrentTime
    
    r1 <- insert $ Role "Администратор"

    r2 <- insert $ Role "Пользователь"

    pass1 <- liftIO $ saltPass "admin"
    insert_ $ User { userEmail = "admin@mail.ru"
                   , userPassword = Just pass1
                   , userRole = r1
                   }

    pass2 <- liftIO $ saltPass "user1"
    insert_ $ User { userEmail = "user1@mail.ru"
                   , userPassword = Just pass2
                   , userRole = r2
                   }

    pass3 <- liftIO $ saltPass "oleg"
    insert_ $ User { userEmail = "oleg@mail.ru"
                   , userPassword = Just pass3
                   , userRole = r1
                   }

    let book1 = Book { bookSeries = "978-5-389-02280-5"
                     , bookTitle = "Записки из подполья"
                     }
                
    bid1 <- insert book1

    let book2 = Book { bookSeries = "978-5-17-097770-3"
                     , bookTitle = "Старик и море"
                     }
                
    bid2 <- insert book2

    let part1 = Part { partName = "Обложка"
                     , partDescr = Nothing
                     }
    pid1 <- insert part1

    let part2 = Part { partName = "Суперобложка"
                     , partDescr = Nothing
                     }
    pid2 <- insert part2

    let part3 = Part { partName = "Титульный лист"
                     , partDescr = Nothing
                     }
    pid3 <- insert part3

    let part4 = Part { partName = "Аннотация"
                     , partDescr = Nothing
                     }
    pid4 <- insert part4

    let part5 = Part { partName = "Предисловие"
                     , partDescr = Nothing
                     }
    pid5 <- insert part5

    let part6 = Part { partName = "Иллюстрация"
                     , partDescr = Nothing
                     }
    pid6 <- insert part6

    let part7 = Part { partName = "Оглавление"
                     , partDescr = Nothing
                     }
    pid7 <- insert part7

    let part8 = Part { partName = "Благодарности"
                     , partDescr = Nothing
                     }
    pid8 <- insert part8

    let part9 = Part { partName = "Глава"
                     , partDescr = Nothing
                     }
    pid9 <- insert part9

    let part10 = Part { partName = "Эпилог"
                      , partDescr = Nothing
                      }
    pid10 <- insert part10

    let part11 = Part { partName = "Послесловие"
                      , partDescr = Nothing
                      }
    pid11 <- insert part11

    let part12 = Part { partName = "Заключение"
                      , partDescr = Nothing
                      }
    pid12 <- insert part12

    return ()
