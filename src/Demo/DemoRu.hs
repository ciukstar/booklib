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
    , User (User, userEmail, userPassword, userRole), Book (Book, bookSeries, bookTitle), Part (Part, partName, partDescr), Topic (Topic, topicName, topicDescr)
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


    let topic1 = Topic { topicName = "Человеческая природа"
                       , topicDescr = Nothing
                       }
    tid1 <- insert topic1

    let topic2 = Topic { topicName = "Свободная воля"
                       , topicDescr = Nothing
                       }
    tid2 <- insert topic2

    let topic3 = Topic { topicName = "Самосознание"
                       , topicDescr = Nothing
                       }
    tid3 <- insert topic3

    let topic4 = Topic { topicName = "Совершеннолетие"
                       , topicDescr = Nothing
                       }
    tid4 <- insert topic4

    let topic5 = Topic { topicName = "Надежда"
                       , topicDescr = Nothing
                       }
    tid5 <- insert topic5

    let topic6 = Topic { topicName = "Ревность"
                       , topicDescr = Nothing
                       }
    tid6 <- insert topic6

    let topic7 = Topic { topicName = "Справедливость"
                       , topicDescr = Nothing
                       }
    tid7 <- insert topic7

    let topic8 = Topic { topicName = "Страх"
                       , topicDescr = Nothing
                       }
    tid8 <- insert topic8

    let topic9 = Topic { topicName = "Свобода"
                       , topicDescr = Nothing
                       }
    tid9 <- insert topic9

    let topic10 = Topic { topicName = "Дружба"
                       , topicDescr = Nothing
                       }
    tid10 <- insert topic10

    let topic11 = Topic { topicName = "Храбрость"
                        , topicDescr = Nothing
                        }
    tid11 <- insert topic11

    let topic12 = Topic { topicName = "Счастье"
                        , topicDescr = Nothing
                        }
    tid12 <- insert topic12

    let topic13 = Topic { topicName = "Страсть"
                        , topicDescr = Nothing
                        }
    tid13 <- insert topic13

    let topic14 = Topic { topicName = "Доброта"
                        , topicDescr = Nothing
                        }
    tid14 <- insert topic14

    let topic15 = Topic { topicName = "Доверие"
                        , topicDescr = Nothing
                        }
    tid15 <- insert topic15

    let topic16 = Topic { topicName = "Война"
                        , topicDescr = Nothing
                        }
    tid16 <- insert topic16

    return ()
