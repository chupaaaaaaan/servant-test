module Main where

import Servant.JS
import Todo

main :: IO ()
main = writeJSForAPI Todo.crud jquery "todo-server/static/todo_crud.js"
