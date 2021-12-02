{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Data.Aeson.Text
import Common.Api

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-35-173-207-244.compute-1.amazonaws.com"
                      5432
                      "ejgrpwsfuuxycg"
                      "daf4205c7d32bd190cb6eaed384791239857c859625054d5194ef53d6d5f3136"
                      "d6es6d556n98vj"

migration :: Query
migration = "CREATE TABLE if not exists tb_usuario (codigoUsuario SERIAL PRIMARY KEY ,username TEXT NOT NULL, senha TEXT NOT NULL);CREATE TABLE if not exists tb_cliente (cdUsuario INTEGER NOT NULL, codigoCliente SERIAL PRIMARY KEY, nome TEXT NOT NULL, telefone TEXT NOT NULL, cpf TEXT NOT NULL, endereco TEXT NOT NULL, FOREIGN KEY (cdUsuario) REFERENCES TB_usuario (codigoUsuario) ON DELETE CASCADE);CREATE TABLE if not exists tb_servicos (cdUsuario INTEGER NOT NULL, codigoServico SERIAL PRIMARY KEY, servico TEXT NOT NULL, valor MONEY NOT NULL, FOREIGN KEY (cdUsuario) REFERENCES TB_usuario (codigoUsuario) ON DELETE CASCADE);CREATE TABLE if not exists tb_agendamento (cdUsuario INTEGER NOT NULL, cdCliente INTEGER NOT NULL, cdServico INTEGER NOT NULL, horario DATE, FOREIGN KEY (cdUsuario) REFERENCES TB_usuario (codigoUsuario) ON DELETE CASCADE,FOREIGN KEY (cdCliente) REFERENCES tb_cliente (codigoCliente) ON DELETE CASCADE,FOREIGN KEY (cdServico) REFERENCES tb_servicos (codigoServico) ON DELETE CASCADE)"


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
            BackendRoute_ClienteListar :/ () -> do
              res :: [Cliente] <- liftIO $ do
                execute_ dbcon migration
                query_ dbcon "SELECT * from tb_cliente"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)
            BackendRoute_Usuario :/ () -> do
              user <- A.decode <$> readRequestBody 2000
              case user of
                Just usuario -> do
                  liftIO $ do
                    execute_ dbcon migration
                    execute dbcon "INSERT INTO tb_usuario (username, senha) VALUES (?,?)"
                            [usuarioUsername usuario, usuarioSenha usuario]
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "error" 
            BackendRoute_Cliente :/ () -> do
              client <- A.decode <$> readRequestBody 2000
              case client of
                Just cliente -> do
                  liftIO $ do
                    execute_ dbcon migration
                    execute dbcon "INSERT INTO tb_cliente (cdUsuario, nome, telefone,cpf,endereco) VALUES (?,?,?,?,?)" (cdUsuarioCliente cliente, clienteNome cliente, clienteTelefone cliente, clienteCpf cliente, clienteEndereco cliente)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "error" 
            _ -> return ()
, _backend_routeEncoder = fullRouteEncoder
}
