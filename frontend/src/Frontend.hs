{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Text as T

import Control.Monad.Fix (MonadFix)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Aeson
import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.


data Pagina = Principal | InsertCli | Clientes | AgendamentoPag | InsertUser 
data Acao = PerfilCliente Int | EditarCliente Int | PerfilServico Int | EditarAgenda Int | Excluir Int


getPath :: R BackendRoute ->  T.Text
getPath r = renderBackendRoute checFullREnc r

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getClienteListReq :: XhrRequest ()
getClienteListReq = xhrRequest "GET" (getPath (BackendRoute_ClienteListar :/ ())) def


getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_ServicosListar :/ ())) def

getUserListReq :: XhrRequest ()
getUserListReq = xhrRequest "GET" (getPath (BackendRoute_UsuarioListar :/ ())) def

getClienteReq :: Int -> XhrRequest()
getClienteReq pid = xhrRequest "GET" (getPath (BackendRoute_ClienteBuscar :/ pid)) def

getExcluirCliente :: Int -> XhrRequest()
getExcluirCliente pid = xhrRequest "GET" (getPath (BackendRoute_ClienteDeletar :/ pid)) def

getExcluirAgendamento :: Int -> XhrRequest()
getExcluirAgendamento pid = xhrRequest "GET" (getPath (BackendRoute_ServicosDeletar :/ pid)) def

getAgendamentosReq :: Int -> XhrRequest()
getAgendamentosReq pid = xhrRequest "GET" (getPath (BackendRoute_ServicosBuscar :/ pid)) def

getServicoReq :: Int -> XhrRequest()
getServicoReq pid = xhrRequest "GET" (getPath (BackendRoute_ServicosBuscar2 :/ pid)) def


editarPerfilCliente :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilCliente pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "div-principal") $ do
    elAttr "div" ("class" =: "col-3 d-flex flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
      el "label" (text "Nome")
      nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE)
      el "label" (text "Telefone")
      telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE)
      el "label" (text "Cpf")
      cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE)
      el "label" (text "Endereço")
      endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) 
    
      let cliente = fmap (\((n,t),(c,e)) -> Cliente  1 0 n t c e) (zipDyn (zipDyn (_inputElement_value nome)(_inputElement_value telefone)) (zipDyn (_inputElement_value cpf)(_inputElement_value endereco)))
      (btn,_) <- elAttr' "button" ("class" =: "btn btn-success mt-3") (text "Editar")
      let submitBtn = domEvent Click btn
      let clientEvt = tag (current cliente) submitBtn
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$>
                performRequestAsync (sendRequest (BackendRoute_ClienteEditar :/ pid)
                <$> clientEvt))

      return ("Editar: " <> (T.pack $ show pid), reqClienteLista <$ submitBtn)  

pagAgendar :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagAgendar pid = Workflow $ do
  elAttr "div" ("class" =: "ml-5") $ do
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
    elAttr "p" ("style" =: "font-size:12px;") (text "(Clique no Mostrar para conseguir agendar)")

    let evt = domEvent Click btn
    cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
    mdyn <- return (switchDyn cli)
    dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

    elAttr "div" ("class" =: "div-principal") $ do
      elAttr "div" ("class" =: "col-3 d-flex flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
        el "label" (text "Nome")
        nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
        el "label" (text "Telefone")
        telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
        el "label" (text "Cpf")
        cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
        el "label" (text "Endereço")
        endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
        id <- numberInputDisable (fmap codigoCliente dynE)
          
        elAttr "div" ("class" =: "d-flex justify-content-center") $ do
          elAttr "div" ("class" =: "col-6 d-flex flex-column") $ do
            elAttr "h3" ("style" =: "text-align: center;") (text "Agendamento")
            el "label" (text "Serviço")
            serv <- inputElement def
            el "label" (text "Valor")
            vl <- numberInput
            el "label" (text "Data do Serviço")
            date <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "date")
              

            let agend = fmap (\((s,v),(d,i))  -> Servicos  1 0 s v d i) (zipDyn  (zipDyn (_inputElement_value serv) (vl))  (zipDyn (_inputElement_value date)(id)))  
            (btn,_) <- elAttr' "button" ("class" =: "btn btn-success mt-3") (text "Agendar")
            let submitBtn = domEvent Click btn
            let clientEvt = tag (current agend) submitBtn
            _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
              (pure never)
              (fmap decodeXhrResponse <$>
                      performRequestAsync (sendRequest (BackendRoute_Servicos :/ ())
                      <$> clientEvt))
            
            return ("Editar: " <> (T.pack $ show pid), reqAgendamentoLista <$ submitBtn) 

editarAgendamento :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
editarAgendamento pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Servicos)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getServicoReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Servicos 1 0 "" 0 "" 0)) <$> mdyn)

  elAttr "div" ("class" =: "div-principal") $ do
    elAttr "div" ("class" =: "col-6 d-flex flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
          el "h3" (text "Agendamento")
          el "label" (text "Serviço")
          serv <- inputElement $ def $ inputElementConfig_setValue .~ (fmap servico dynE)
          el "label" (text "Valor")
          vl <- numberInputDyn (fmap valor dynE) 
          el "label" (text "Data do Serviço")
          date <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "date")
          cdserv <- numberInputDisable (fmap codigoServico dynE)
    
          let agend = fmap (\((s,v),(d,i))  -> Servicos  1 0 s v d i) (zipDyn  (zipDyn (_inputElement_value serv) (vl))  (zipDyn (_inputElement_value date)(cdserv)))  
          (btn,_) <- elAttr' "button" ("class" =: "btn btn-success mt-3") (text "Agendar")
          let submitBtn = domEvent Click btn
          let clientEvt = tag (current agend) submitBtn
          _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$>
                  performRequestAsync (sendRequest (BackendRoute_ClienteEditar :/ pid)
                      <$> clientEvt))

          return ("Editar: " <> (T.pack $ show pid), reqClienteLista <$ submitBtn)


pagPerfilCliente :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilCliente pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "div-principal") $ do
    elAttr "div" ("class" =: "col-3 d-flex flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
      el "label" (text "Nome")
      nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Telefone")
      telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Cpf")
      cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Endereço")
      endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      id <- numberInputDisable (fmap codigoCliente dynE)   
      
      (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
      let submitBtn = domEvent Click btn

      return ("Voltar: " <> (T.pack $ show pid), reqAgendamentoLista <$ submitBtn)  

pagPerfilClienteAgendamento :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilClienteAgendamento pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getAgendamentosReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "div-principal") $ do
    elAttr "div" ("class" =: "col-3 d-flex flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
      el "label" (text "Nome")
      nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Telefone")
      telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Cpf")
      cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Endereço")
      endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      id <- numberInputDisable (fmap codigoCliente dynE)   
      
      (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
      let submitBtn = domEvent Click btn

      return ("Voltar: " <> (T.pack $ show pid), reqAgendamentoLista <$ submitBtn)  

pagExcluirCliente :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagExcluirCliente pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #dc3545;") (text "Excluir")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getExcluirCliente pid) <$> evt))
  
    
    
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
  let submitBtn = domEvent Click btn

  return ("Voltar: " <> (T.pack $ show pid), reqAgendamentoLista <$ submitBtn)  

pagExcluirAgendamento :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagExcluirAgendamento pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #dc3545;") (text "Excluir")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getExcluirAgendamento pid) <$> evt))
  
    
    
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
  let submitBtn = domEvent Click btn

  return ("Voltar: " <> (T.pack $ show pid), reqAgendamentoLista <$ submitBtn)  

reqListaClient :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqListaClient = do
  r <- workflow reqClienteLista
  el "div" (dynText r)



reqCliente :: (DomBuilder t m, Prerender js t m) => m ()
reqCliente = do
  elAttr "div" ("class" =: "div-principal") $ do
    elAttr "div" ("class" =: "col-4 d-flex flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
      el "label" (text "Nome: ")
      nome <- inputElement def
      el "label" (text "Telefone: ")
      telefone <- inputElement def
      el "label" (text "CPF: ")
      cpf <- inputElement def
      el "label" (text "endereço: ")
      endereco <- inputElement def
      let cliente = fmap (\((n,t),(c,e)) -> Cliente  1 0 n t c e) (zipDyn (zipDyn (_inputElement_value nome)(_inputElement_value telefone)) (zipDyn (_inputElement_value cpf)(_inputElement_value endereco)))
      (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-primary mt-3") (text "Inserir")
      let click = domEvent Click submitBtn
      let clienteEvt = tag (current cliente) click
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> clienteEvt ))
      return  ()

tabCliente :: (PostBuild t m, DomBuilder t m) => Dynamic t Cliente -> m (Event t Acao)
tabCliente cl = do
  el "tr" $ do
    el "td" (dynText $ fmap clienteNome cl)
    el "td" (dynText $ fmap clienteTelefone cl)
    el "td" (dynText $ fmap clienteCpf cl)
    el "td" (dynText $ fmap clienteEndereco cl)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Agendar")
    let evt = (fmap (const PerfilCliente)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-warning" <> "style" =: "background: #ffc107; margin-left: 15px;") (text "Editar")
    let evt2 = (fmap (const EditarCliente)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #dc3545; margin-left: 15px;") (text "Excluir")
    let evt3 = (fmap (const Excluir)) (domEvent Click btn)
    return (attachPromptlyDynWith (flip ($)) (fmap codigoCliente cl) (leftmost [evt, evt2,evt3]))

reqClienteLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Workflow t m T.Text
reqClienteLista = Workflow $ do
  el "div" $ do
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Listar")
    let click = domEvent Click btn
    clients :: Dynamic t (Event t (Maybe [Cliente])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getClienteListReq <$> click))
    evt <- return (fmap (fromMaybe []) $ switchDyn clients)
    dynCli <- foldDyn (++) [] evt
    tb <- elAttr "table" ("class" =: "table") $ do
      el "thead" $ do
        el "tr" $ do
          elAttr "th" ("scope" =: "col") (text "Nome")
          elAttr "th" ("scope" =: "col") (text "Telefone")
          elAttr "th" ("scope" =: "col") (text "CPF")
          elAttr "th" ("scope" =: "col") (text "Endereço")
          elAttr "th" ("scope" =: "col") (text "")


      el "tbody" $ do
        simpleList dynCli tabCliente
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
      escolherPag (PerfilCliente pid) = pagAgendar pid
      escolherPag (EditarCliente pid) = editarPerfilCliente pid
      escolherPag (Excluir pid) = pagExcluirCliente pid

reqListaAgend :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqListaAgend = do 
    r <- workflow reqAgendamentoLista
    el "div" (dynText r)

tabAgendamento :: (PostBuild t m, DomBuilder t m) => Dynamic t Agendamento -> m (Event t Acao)
tabAgendamento ag = do
  el "tr" $ do
    el "td" (dynText $ fmap (T.pack . show . codigoServicoAgendemento) ag)
    el "td" (dynText $ fmap nomeAgendamento ag)
    el "td" (dynText $ fmap telefoneAgendamento ag)
    el "td" (dynText $ fmap cpfAgendamento ag)
    el "td" (dynText $ fmap enderecoAgendamento ag)
    el "td" (dynText $ fmap servicoAgendamento ag)
    el "td" (dynText $ fmap dataAgendamento ag)
    el "td" (dynText $ fmap(T.pack . show . valorAgendamento) ag)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-warning" <> "style" =: "background: #ffc107; margin-left: 15px;") (text "Editar")
    let evt = (fmap (const EditarAgenda)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #dc3545; margin-bottom: 10px; margin-left: 15px;") (text "Excluir")
    let evt2 = (fmap (const Excluir)) (domEvent Click btn)
    return (attachPromptlyDynWith (flip ($)) (fmap codigoServicoAgendemento ag) (leftmost [evt,evt2]))

reqAgendamentoLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Workflow t m T.Text
reqAgendamentoLista = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Listar")
  let click = domEvent Click btn
  agend :: Dynamic t (Event t (Maybe [Agendamento])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
  evt <- return (fmap (fromMaybe []) $ switchDyn agend)
  dynAgenda <- foldDyn (++) [] evt
  tb <- elAttr "table" ("class" =: "table") $ do
    el "thead" $ do
      el "tr" $ do
        elAttr "th" ("scope" =: "col") (text "Codigo Agendamento")
        elAttr "th" ("scope" =: "col") (text "Nome do Cliente")
        elAttr "th" ("scope" =: "col") (text "Telefone")
        elAttr "th" ("scope" =: "col") (text "CPF")
        elAttr "th" ("scope" =: "col") (text "Endereço")
        elAttr "th" ("scope" =: "col") (text "Serviço")
        elAttr "th" ("scope" =: "col") (text "Data")
        elAttr "th" ("scope" =: "col") (text "Valor")

    el "tbody" $ do
      simpleList dynAgenda tabAgendamento
  tb' <- return $ switchDyn $ fmap leftmost tb
  return ("Listagem", escolherPag <$> tb')
  where
    escolherPag (EditarAgenda pid) = editarAgendamento pid
    escolherPag (Excluir pid) = pagExcluirAgendamento pid



reqUsuario :: ( DomBuilder t m
        , Prerender js t m
        ) => m ()
reqUsuario = do
  elAttr "div" ("class" =: "div-principal") $ do
  elAttr "div" ("class" =: "d-flex flex-column col-4 flex-column shadow-lg p-3 mb-5 bg-white rounded") $ do
    elAttr "div" ("class" =: "d-flex justify-content-center") $ do
      elAttr "img" ("id" =: "logo" <> "class" =: "navbar-brand" <> "src" =: static @"Logo.png") blank
    el "label" (text "Usuario") 
    username <- inputElement def
    el "label" (text "Senha") 
    senha <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "password")
    let user = fmap (\(u,s) -> Usuario 0 u s) (zipDyn (_inputElement_value username)(_inputElement_value  senha))
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-primary mt-2") (text "Inserir")
    let click = domEvent Click submitBtn
    let userEvt = tag (current user) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Usuario :/ ()) <$> userEvt ))
    return ()

tabUsuario :: DomBuilder t m => Usuario -> m ()
tabUsuario us = do
  el "tr" $ do
    el "td" (text $ usuarioUsername us)
    el "td" (text $ usuarioSenha us)



reqUsuarioLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqUsuarioLista = do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Listar")
  let click = domEvent Click btn
  users :: Dynamic t (Event t (Maybe [Usuario])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getUserListReq <$> click))
  dynUser <- foldDyn (\ps d -> case ps of 
                                Nothing -> []
                                Just s -> d++s) [] (switchDyn users )
  elAttr "table" ("class" =: "table") $ do
    el "thead" $ do
      el "tr" $ do
        elAttr "th" ("scope" =: "col") (text "Usuario")
        elAttr "th" ("scope" =: "col") (text "Senha")

    el "tbody" $ do
      dyn_ (fmap sequence (ffor dynUser (fmap tabUsuario)))


clickli :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickli p t = do
  (ev, _) <- elAttr' "li" ("class" =: "nav-item") (elAttr "a" ("href" =: "#" <> "class" =: "nav-link") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
  case p of
    Principal -> blank
    InsertCli -> reqCliente
    Clientes -> reqListaClient
    AgendamentoPag -> reqListaAgend
    InsertUser -> reqUsuario

homepag :: (DomBuilder t m , PostBuild t m, MonadHold t m,MonadFix m, Prerender js t m) => m ()
homepag = do
  pagina <- el "div" menu
  dyn_ $ currPag <$> pagina

login :: (DomBuilder t m , PostBuild t m, MonadHold t m, Prerender js t m) => m ()
login = do
  el "h1" (text "Login") 
  el "label" (text "Username") 
  username <- inputElement def
  el "label" (text "Senha") 
  senha <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "password")
  text " "

catpag :: (DomBuilder t m , PostBuild t m, MonadHold t m, Prerender js t m) => m ()
catpag = do
  el "h1" (text "Pagina Categorias") 
  el "span" (text "Pagina em construcao")


prodpag :: (DomBuilder t m , PostBuild t m, MonadHold t m, Prerender js t m) => m ()
prodpag = do
  el "h1" (text "Pagina Produtos") 
  el "span" (text "Pagina em construcao")


menu :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menu = do
  evs <- elAttr "ul" ("style" =:"background: #bcd8fe;" ) $ do
    elAttr "nav" ("class" =: "navbar navbar-expand-lg navbar-light bg-light" <> "style" =: "background: #bcd8fe !important;") $ do
      elAttr "div" ("class" =: "container-fluid") $ do
        elAttr "img" ("id" =: "logo" <> "class" =: "navbar-brand" <> "src" =: static @"Logo.png") (text "AgendaDeServiços.com")
        elAttr "button" ("class" =: "navbar-toggler" <> "type" =: "button" <> "data-bs-toggle" =:"collapse" <> "data-bs-target" =: "#navbarSupportedContent" <> "aria-controls" =: "navbarSupportedContent" <> "aria-expanded" =: "false" <> "aria-label" =: "Toggle navigation" ) $ do
          elAttr "span" ("class" =: "navbar-toggle-icon") blank
        elAttr "div" ("class" =: "navbar-collapse" <> "id" =: "navbarSupporedContent") $ do
          elAttr "ul" ("class" =: "navbar-nav me-auto mb-2 mb-lg-0") $ do
            
            p2 <- clickli InsertCli "Inserir Cliente"
            p3 <- clickli Clientes "Clientes"
            p4 <- clickli AgendamentoPag "Agendamento"
            p5 <- clickli InsertUser "Inserir Usuario"
            return (leftmost [p2,p3,p4,p5])
  holdDyn Principal evs



numberInput :: (Read a, Num a) => DomBuilder t m => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n
numberInputDisable :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDisable p = do
    val <- return (fmap (T.pack . show) p)
    n <- inputElement $ def
      & inputElementConfig_setValue .~ val
      & inputElementConfig_elementConfig 
      . elementConfig_initialAttributes .~ ("disabled" =: "teste" <> "type" =: "hidden")
    return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                  (_inputElement_value n)

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDyn p = do
    val <- return (fmap (T.pack . show) p)
    n <- inputElement $ def
      & inputElementConfig_setValue .~ val
    return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                  (_inputElement_value n)
      



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "AgendaDeServiços.com"
      elAttr "link" ("href" =: static @"main.css" 
              <> "type" =: "text/css" 
              <> "rel" =: "stylesheet") blank

      elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"
                    <> "rel" =: "stylesheet"
                    <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                    <> "crossorigin" =:"anonymous") blank

      elAttr "script" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"
                    <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                    <> "crossorigin" =:"anonymous") blank
  , _frontend_body = do
      homepag

      elAttr "script" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"
                    <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                    <> "crossorigin" =:"anonymous") blank
  }
