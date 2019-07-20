module Main where


import Control.Monad (void)
import Control.Monad.IO.Class
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Todo

getTodoAll  :: ClientM [Todo]
postTodo    :: Todo -> ClientM Todo
putTodoId   :: Int -> Todo -> ClientM ()
deleteTodo  :: Int -> ClientM ()

getTodoAll :<|> postTodo :<|> putTodoId :<|> deleteTodo = client Todo.crud

todoList :: [Todo]
todoList =
  [ Todo 1 "ElasticsearchのAPIについてまとめる" False
  , Todo 2 "Javaの文法について勉強" False
  , Todo 3 "溜まっている洗濯物を片付ける" True
  ]

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "localhost" 8080 ""
  void . flip runClientM env $ do
    mapM_ postTodo todoList
    putTodoId 1 $ (todoList !! 0) { done = True }
    deleteTodo 3
    list <- getTodoAll
    liftIO . mapM_ putStrLn $ map title list

