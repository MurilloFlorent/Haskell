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
getConn = ConnectInfo "ec2-54-205-232-84.compute-1.amazonaws.com"
                      5432
                      "svvgaadgjwwjbb"
                      "155c4c74a17c9c1d9a7813b4d2617fec49405dd64d1a3072e29033046dd9dd92"
                      "donred3bpfpg5"

migrationUsuario :: Query
migrationUsuario = "CREATE TABLE if not exists tb_usuario (codigoUsuario SERIAL PRIMARY KEY ,username TEXT NOT NULL, senha TEXT NOT NULL)"

migrationCliente :: Query
migrationCliente = "CREATE TABLE if not exists tb_cliente (cdUsuario INTEGER NOT NULL, codigoCliente SERIAL PRIMARY KEY, nome TEXT NOT NULL, telefone TEXT NOT NULL, cpf TEXT NOT NULL, endereco TEXT NOT NULL, FOREIGN KEY (cdUsuario) REFERENCES TB_usuario (codigoUsuario) ON DELETE CASCADE)"

migrationServicos :: Query
migrationServicos = "CREATE TABLE if not exists tb_servicos (cdUsuario INTEGER NOT NULL, codigoServico SERIAL PRIMARY KEY, servico TEXT NOT NULL, valor REAL NOT NULL, FOREIGN KEY (cdUsuario) REFERENCES tb_usuario (codigoUsuario) ON DELETE CASCADE)"

migrationAgendamento :: Query
migrationAgendamento = "CREATE TABLE if not exists tb_agendamento (cdUsuario INTEGER NOT NULL, cdCliente INTEGER NOT NULL, cdServico INTEGER NOT NULL, horario DATE, FOREIGN KEY (cdUsuario) REF0ERENCES tb_usuario (codigoUsuario) ON DELETE CASCADE,FOREIGN KEY (cdCliente) REFERENCES tb_cliente (codigoCliente) ON DELETE CASCADE,FOREIGN KEY (cdServico) REFERENCES tb_servicos (codigoServico) ON DELETE CASCADE)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
            BackendRoute_Buscar :/ pid -> method GET $ do
              res :: [Cliente] <- liftIO $ do
                execute_ dbcon migrationCliente
                query dbcon "SELECT * from tb_cliente where codigoCliente=?"(Only (pid :: Int))
              if res /= [] then do
                modifyResponse setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText (Prelude.head res))
              else
                modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_UsuarioListar :/ () -> method GET $ do
              res :: [Usuario] <- liftIO $ do
                execute_ dbcon migrationUsuario
                query_ dbcon "SELECT * from tb_usuario"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)
            BackendRoute_ServicosListar :/ () -> method GET $ do
              res :: [Servicos] <- liftIO $ do
                execute_ dbcon migrationServicos
                query_ dbcon "SELECT * from tb_servicos"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)
            BackendRoute_ClienteListar :/ () -> method GET $ do
              res :: [Cliente] <- liftIO $ do
                execute_ dbcon migrationCliente
                query_ dbcon "SELECT * from tb_cliente"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)
            BackendRoute_Usuario :/ () -> method POST $ do
              user <- A.decode <$> readRequestBody 2000
              case user of
                Just usuario -> do
                  liftIO $ do
                    execute_ dbcon migrationUsuario
                    execute dbcon "INSERT INTO tb_usuario (username, senha) VALUES (?,?)"
                            [usuarioUsername usuario, usuarioSenha usuario]
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "error" 
            BackendRoute_Cliente :/ () -> method POST $  do
              client <- A.decode <$> readRequestBody 2000
              case client of
                Just cliente -> do
                  liftIO $ do
                    execute_ dbcon migrationCliente
                    execute dbcon "INSERT INTO tb_cliente (cdUsuario, nome, telefone,cpf,endereco) VALUES (?,?,?,?,?)" (cdUsuarioCliente cliente, clienteNome cliente, clienteTelefone cliente, clienteCpf cliente, clienteEndereco cliente)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "error" 
            BackendRoute_Servicos :/ () -> method POST $  do
              serv <- A.decode <$> readRequestBody 2000
              case serv of
                Just servicos -> do
                  liftIO $ do
                    execute_ dbcon migrationServicos
                    execute dbcon "INSERT INTO tb_servicos (cdUsuario, servico, valor) VALUES (?,?,?)" (cdUsuario servicos, servico servicos, valor servicos)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "error" 
            _ -> return ()
, _backend_routeEncoder = fullRouteEncoder
}
