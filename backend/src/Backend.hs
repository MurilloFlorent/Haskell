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
getConn = ConnectInfo "ec2-34-233-214-228.compute-1.amazonaws.com"
                      5432
                      "pdindoopmdpwdk"
                      "4cd9a084184b8af9302d393f8ad427e7d4ca1b9b7041875f8ff385cd6f08c93d"
                      "dblkvm4b8s6j0u"

migrationUsuario :: Query
migrationUsuario = "CREATE TABLE if not exists tb_usuario (codigoUsuario SERIAL PRIMARY KEY ,username TEXT NOT NULL, senha TEXT NOT NULL)"

migrationCliente :: Query
migrationCliente = "CREATE TABLE if not exists tb_cliente (cdUsuario INTEGER NOT NULL, codigoCliente SERIAL PRIMARY KEY, nome TEXT NOT NULL, telefone TEXT NOT NULL, cpf TEXT NOT NULL, endereco TEXT NOT NULL, FOREIGN KEY (cdUsuario) REFERENCES TB_usuario (codigoUsuario) ON DELETE CASCADE)"

migrationServicos :: Query
migrationServicos = "CREATE TABLE if not exists tb_servicos (cdUsuario INTEGER NOT NULL, codigoServico SERIAL PRIMARY KEY, servico TEXT NOT NULL, valor REAL NOT NULL,data TEXT, idCategoria INTEGER, FOREIGN KEY (cdUsuario) REFERENCES tb_usuario (codigoUsuario) ON DELETE CASCADE)"

migrationAgendamento :: Query
migrationAgendamento = "CREATE TABLE if not exists tb_agendamento (cdAgendamento SERIAL PRIMARY KEY, cdUsuario INTEGER NOT NULL, cdCliente INTEGER NOT NULL, servico TEXT NOT NULL, valor REAL NOT NULL, data DATE,  FOREIGN KEY (cdUsuario) REFERENCES tb_usuario (codigoUsuario) ON DELETE CASCADE)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
            BackendRoute_ClienteBuscar :/ pid -> do
                res :: [Cliente] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "SELECT * FROM tb_cliente where codigoCliente = ?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_ClienteDeletar :/ pid -> do
                res :: [Cliente] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "DELETE FROM tb_cliente where codigoCliente = ?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_ServicosBuscar :/ pid -> do
                res :: [Servicos] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "select * from tb_servicos as a inner join tb_cliente on idcategoria = codigocliente where codigoservico = ?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_ServicosBuscar2 :/ pid -> do
                res :: [Servicos] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "select * from tb_servicos where codigoservico = ?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_ServicosDeletar :/ pid -> do
                res :: [Servicos] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "DELETE FROM tb_servicos where codigoServico = ?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_ServicosEditar :/ pid -> do
                serv <- A.decode <$> readRequestBody 2000
                case serv of
                  Just servicos -> do
                    liftIO $ do
                      execute_ dbcon migrationServicos
                      execute dbcon "UPDATE tb_servicos SET servico ='?', valor = ? ,data=? WHERE  codigoServico = ?" (servico servicos, valor servicos,date servicos, codigoServico servicos)
                    modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERROR"  
            BackendRoute_ClienteEditar :/ pid -> do
                client <- A.decode <$> readRequestBody 2000
                case client of
                  Just cliente -> do
                    liftIO $ do
                      execute_ dbcon migrationCliente
                      execute dbcon "UPDATE tb_cliente SET nome = ?,  telefone= ?, cpf= ?, endereco=? WHERE  codigoCliente = ?" (clienteNome cliente, clienteTelefone cliente, clienteCpf cliente, clienteEndereco cliente, pid)
                    modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERROR"  
            BackendRoute_UsuarioListar :/ () -> method GET $ do
              res :: [Usuario] <- liftIO $ do
                execute_ dbcon migrationUsuario
                query_ dbcon "SELECT * from tb_usuario"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)
            BackendRoute_ServicosListar :/ () -> method GET $ do
              res :: [Agendamento] <- liftIO $ do
                execute_ dbcon migrationServicos
                query_ dbcon "select a.cdusuario,a.codigoservico, a.servico, a.valor, a.data, a.idcategoria, b.nome, b.telefone, b.cpf, b.endereco from tb_servicos as a inner join tb_cliente as b on a.idcategoria = b.codigocliente"
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
                    execute dbcon "INSERT INTO tb_servicos (cdUsuario, servico, valor,data,idcategoria) VALUES (?,?,?,?,?)" (cdUsuario servicos, servico servicos, valor servicos, date servicos, idcategoria servicos)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "error" 
            _ -> return ()
, _backend_routeEncoder = fullRouteEncoder
}
