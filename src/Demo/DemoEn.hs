{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.Time.Clock (getCurrentTime, addUTCTime)

import Database.Persist (PersistStoreWrite (insert, insert_, insertKey))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( Role(Role)
    , User (User, userEmail, userPassword, userRole), Book (Book, bookTitle, bookSeries), Part (Part, partName, partDescr)
    )
    
import System.Random (randomRIO)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

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

    let book1 = Book { bookSeries = "9781857152715"
                     , bookTitle = "Notes From The Underground"
                     }
                
    bid1 <- insert book1

    let book2 = Book { bookSeries = "9780684163260"
                     , bookTitle = "The Old Man and the Sea"
                     }
                
    bid2 <- insert book2

    let part1 = Part { partName = "Half title"
                     , partDescr = Nothing
                     }
    pid1 <- insert part1

    let part2 = Part { partName = "Frontispiece"
                     , partDescr = Nothing
                     }
    pid2 <- insert part2

    let part3 = Part { partName = "Title page"
                     , partDescr = Nothing
                     }
    pid3 <- insert part3

    let part4 = Part { partName = "Copyright page"
                     , partDescr = Nothing
                     }
    pid4 <- insert part4

    let part5 = Part { partName = "Dedication"
                     , partDescr = Nothing
                     }
    pid5 <- insert part5

    let part6 = Part { partName = "Epigraph"
                     , partDescr = Nothing
                     }
    pid6 <- insert part6

    let part7 = Part { partName = "Table of Contents"
                     , partDescr = Nothing
                     }
    pid7 <- insert part7

    let part8 = Part { partName = "Foreword"
                     , partDescr = Nothing
                     }
    pid8 <- insert part8

    let part9 = Part { partName = "Preface"
                     , partDescr = Nothing
                     }
    pid9 <- insert part9

    let part10 = Part { partName = "Acknowledgments"
                      , partDescr = Nothing
                      }
    pid10 <- insert part10

    let part11 = Part { partName = "Chapter"
                      , partDescr = Nothing
                      }
    pid11 <- insert part11

    let part12 = Part { partName = "Epilogue"
                      , partDescr = Nothing
                      }
    pid12 <- insert part12

    let part13 = Part { partName = "Afterword"
                      , partDescr = Nothing
                      }
    pid13 <- insert part13

    let part14 = Part { partName = "Conclusion"
                      , partDescr = Nothing
                      }
    pid14 <- insert part14
    

    return ()
