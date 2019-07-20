{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo
  ( Todo(..)
  , CRUD
  , crud
  ) where

import GHC.Generics
import Data.Aeson
import Data.Proxy
import Servant.API
import Web.FormUrlEncoded(FromForm(..), parseUnique)

data Todo = Todo
  { todoId :: Int
  , title :: String
  , done :: Bool
  } deriving (Generic, FromJSON, ToJSON)

instance FromForm Todo where
  fromForm form = Todo
                  <$> parseUnique "todoId" form
                  <*> parseUnique "title" form
                  <*> parseUnique "done" form


type CRUD = "todo" :> "all" :> Get '[JSON][Todo]
            :<|> "todo" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
            :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
            :<|> "todo" :> Capture "id" Int :> Delete '[JSON] ()

crud :: Proxy CRUD
crud = Proxy
