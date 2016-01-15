{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

{- http://degoes.net/articles/modern-fp/ -}

module Lib
    ( someFunc
    )
    where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (log)

type Bytes = String
type Path = String

data Level where
  Debug :: Level
  Info :: Level
  deriving (Show)
  
-- api for cloud files
class (Monad m) => MonadCloud m where
  saveFile :: FilePath -> Bytes -> m ()
  listFiles :: FilePath -> m [Path]
  
-- api for logging
class (Monad m) => MonadLog m where
  log :: Level -> String -> m ()
  
-- api for REST client
class MonadRest m where
  get :: Path -> m Bytes
  put :: Path -> Bytes -> m Bytes
  
-- an instrumentation that adds logging to every call.
newtype CloudLogT m a = CloudLogT { runCloudLogT :: m a } deriving (Functor, Applicative, Monad, MonadLog)

instance (MonadLog m, MonadCloud m) => MonadCloud (CloudLogT m) where
  saveFile p bytes = do log Debug  ("saveFile: " ++ p)
                        lift $ saveFile p bytes
  listFiles p = do log Debug ("listFiles: " ++ p)
                   lift $ listFiles p
                   
instance MonadTrans CloudLogT where
  lift = CloudLogT

-- log to standard out
newtype StdOutLogT m a = StdOutLogT { runStdOutLogT :: m a } deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadLog (StdOutLogT m) where
  log l m = liftIO $ putStrLn $ "[" ++ show (l) ++ "] : " ++ m

-- implementation of MonadCloud that uses a REST client
newtype CloudRestT m a = CloudRestT { runCloudRestT :: m a} deriving (Functor, Applicative, Monad, MonadRest, MonadLog)

instance (Monad m, MonadRest m) => MonadCloud (CloudRestT m) where
  saveFile p bytes = do _ <- put ("/file/" ++ p) bytes
                        return ()
  listFiles p = do _ <- get ("/files/" ++ p)
                   return ["MockFile"]

-- A (non-functional) REST client
newtype RestClientT m a = RestClientT { runRestClientT :: m a } deriving (Functor, Applicative, Monad, MonadIO, MonadLog)

instance (MonadIO m) => MonadRest (RestClientT m) where
  get p = do liftIO $ putStrLn $ "GET " ++ p
             return []
  put p bytes = do liftIO $ putStrLn $ "PUT " ++ p ++ " : " ++ bytes
                   return []

-- application
app :: (MonadCloud m, MonadLog m) => m ()
app = do [f] <- listFiles "/home/smunix"
         (log Info) . ("Found " ++) $ f
         flip saveFile "smunix" $ f
         return ()

someFunc :: IO ()
someFunc = runStdOutLogT $ runRestClientT $ runCloudRestT $ runCloudLogT app
-- someFunc = runStdOutLogT $ runRestClientT $ runCloudRestT app
