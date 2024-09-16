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
    , User (User, userEmail, userPassword, userRole), Book (Book, bookTitle, bookSeries), Part (Part, partName, partDescr), Topic (Topic, topicName, topicDescr)
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


    let topic1 = Topic { topicName = "Human nature"
                       , topicDescr = Nothing
                       }
    tid1 <- insert topic1

    let topic2 = Topic { topicName = "Free will"
                       , topicDescr = Nothing
                       }
    tid2 <- insert topic2

    let topic3 = Topic { topicName = "Self-awareness"
                       , topicDescr = Nothing
                       }
    tid3 <- insert topic3

    let topic4 = Topic { topicName = "Coming of age"
                       , topicDescr = Nothing
                       }
    tid4 <- insert topic4

    let topic5 = Topic { topicName = "Hope"
                       , topicDescr = Nothing
                       }
    tid5 <- insert topic5

    let topic6 = Topic { topicName = "Jealousy"
                       , topicDescr = Nothing
                       }
    tid6 <- insert topic6

    let topic7 = Topic { topicName = "Justice"
                       , topicDescr = Nothing
                       }
    tid7 <- insert topic7

    let topic8 = Topic { topicName = "Fear"
                       , topicDescr = Nothing
                       }
    tid8 <- insert topic8

    let topic9 = Topic { topicName = "Freedom"
                       , topicDescr = Nothing
                       }
    tid9 <- insert topic9

    let topic10 = Topic { topicName = "Friendship"
                       , topicDescr = Nothing
                       }
    tid10 <- insert topic10

    let topic11 = Topic { topicName = "Bravery"
                        , topicDescr = Nothing
                        }
    tid11 <- insert topic11

    let topic12 = Topic { topicName = "Happiness"
                        , topicDescr = Nothing
                        }
    tid12 <- insert topic12

    let topic13 = Topic { topicName = "Passion"
                        , topicDescr = Nothing
                        }
    tid13 <- insert topic13

    let topic14 = Topic { topicName = "Kindness"
                        , topicDescr = Nothing
                        }
    tid14 <- insert topic14

    let topic15 = Topic { topicName = "Trust"
                        , topicDescr = Nothing
                        }
    tid15 <- insert topic15

    let topic16 = Topic { topicName = "War"
                        , topicDescr = Nothing
                        }
    tid16 <- insert topic16
    

    return ()
