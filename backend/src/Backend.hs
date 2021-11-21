{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-35-173-207-244.compute-1.amazonaws.com"
                      5432
                      "ejgrpwsfuuxycg"
                      "daf4205c7d32bd190cb6eaed384791239857c859625054d5194ef53d6d5f3136"
                      "d6es6d556n98vj"

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
            BackendRoute_Cliente :/ () -> do
              Just nome <- A.decode <$> readRequestBody 2000
              liftIO $ do
                    execute_ dbcon migration
                    execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
              modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
, _backend_routeEncoder = fullRouteEncoder
}
