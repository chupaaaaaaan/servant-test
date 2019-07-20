{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Proxy
import Servant.API
import Servant.API.ContentTypes
import Servant.Server
import Servant.Server.StaticFiles
import Network.HTTP.Media((//),(/:))
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Logger
import Todo


type API = Get '[HTML] ByteString
           :<|> "static" :> Raw
           :<|> Todo.CRUD

api :: Proxy API
api = Proxy


data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ bs = bs

server :: ByteString -> TVar (Int, IntMap Todo) -> Server API
server indexHtml db = index
  :<|> serveDirectoryFileServer "todo-server/static"
  :<|> getTodoAll
  :<|> postTodo
  :<|> putTodoId
  :<|> deleteTodoId
  where
    index = pure indexHtml
    getTodoAll = liftIO $ IntMap.elems . snd <$> atomically (readTVar db)
    postTodo todo = liftIO . atomically $ do
      (maxId, m) <- readTVar db
      let newId = maxId + 1
          newTodo = todo {todoId = newId}
      writeTVar db (newId, IntMap.insert newId newTodo m)
      pure newTodo
    putTodoId tid todo = liftIO . atomically . modifyTVar db $
      \(maxId, m) -> (maxId, IntMap.insert tid todo m)
    deleteTodoId tid = liftIO. atomically . modifyTVar db $
      \(maxId, m) -> (maxId, IntMap.delete tid m)
      

main :: IO ()
main = do
  db <- atomically $ newTVar (0, IntMap.empty)
  indexHtml <- B.readFile "todo-server/templates/index.html"
  Logger.withStdoutLogger $ \logger -> do
    putStrLn "Listening on port 8080"
    let settings = ( Warp.setPort   8080 .
                     Warp.setLogger logger
                   ) Warp.defaultSettings
    Warp.runSettings settings $ serve api (server indexHtml db)
