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


data Pagina = Principal | Usuarios | Clientes | ServicosPag | InsertUser | InsertCli | InsertServ
data Acao = PerfilCliente Int | EditarCliente Int | PerfilServico Int | EditarServico Int


getPath :: R BackendRoute ->  T.Text
getPath r = renderBackendRoute checFullREnc r

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getClienteListReq :: XhrRequest ()
getClienteListReq = xhrRequest "GET" (getPath (BackendRoute_ClienteListar :/ ())) def

getServiceListReq :: XhrRequest ()
getServiceListReq = xhrRequest "GET" (getPath (BackendRoute_ServicosListar :/ ())) def

getUserListReq :: XhrRequest ()
getUserListReq = xhrRequest "GET" (getPath (BackendRoute_UsuarioListar :/ ())) def

getClienteReq :: Int -> XhrRequest()
getClienteReq pid = xhrRequest "GET" (getPath (BackendRoute_ClienteBuscar :/ pid)) def

getServicosReq :: Int -> XhrRequest()
getServicosReq pid = xhrRequest "GET" (getPath (BackendRoute_ServicosBuscar :/ pid)) def


editarPerfilCliente :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilCliente pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "col-3 d-flex flex-column") $ do
    el "label" (text "Nome")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE)
    el "label" (text "Telefone")
    telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE)
    el "label" (text "Cpf")
    cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE)
    el "label" (text "Endereco")
    endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE)
  
    let cliente = fmap (\((n,t),(c,e)) -> Cliente  1 0 n t c e) (zipDyn (zipDyn (_inputElement_value nome)(_inputElement_value telefone)) (zipDyn (_inputElement_value cpf)(_inputElement_value endereco)))
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Editar")
    let submitBtn = domEvent Click btn
    let clientEvt = tag (current cliente) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$>
              performRequestAsync (sendRequest (BackendRoute_ClienteEditar :/ pid)
              <$> clientEvt))

    return ("Editar: " <> (T.pack $ show pid), reqClienteLista <$ submitBtn)  
    where



reqListaClient :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqListaClient = do
  r <- workflow reqClienteLista
  el "div" (dynText r)


pagPerfilCliente :: ( DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilCliente pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  prod :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- holdDyn Nothing (switchDyn prod)
  dynCli <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)
  el "div" $ do
    elAttr "div" ("class" =: "d-flex flex-row align-items-center") $ do
      el "h5" (text "Nome:")
      el "h5" (dynText $ fmap clienteNome dynCli)
    elAttr "div" ("class" =: "d-flex flex-row align-items-center") $ do
      el "h5" (text "Telefone:")
      el "h5" (dynText $ fmap clienteTelefone dynCli)
    elAttr "div" ("class" =: "d-flex flex-row align-items-center") $ do
      el "h5" (text "CPF:")
      el "h5" (dynText $ fmap clienteCpf dynCli)
    elAttr "div" ("class" =: "d-flex flex-row align-items-center") $ do
      el "h5" (text "Endereco:")
      el "h5" (dynText $ fmap clienteEndereco dynCli)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger") (text "Voltar")
    let ret = domEvent Click btn
    return ("Perfil: " <> (T.pack $ show pid), reqClienteLista <$ ret)

reqCliente :: (DomBuilder t m, Prerender js t m) => m ()
reqCliente = do
  nome <- inputElement def
  telefone <- inputElement def
  cpf <- inputElement def
  endereco <- inputElement def
  let cliente = fmap (\((n,t),(c,e)) -> Cliente  1 0 n t c e) (zipDyn (zipDyn (_inputElement_value nome)(_inputElement_value telefone)) (zipDyn (_inputElement_value cpf)(_inputElement_value endereco)))
  (submitBtn,_) <- el' "button" (text "Inserir")
  let click = domEvent Click submitBtn
  let clienteEvt = tag (current cliente) click
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> clienteEvt ))
  return()

tabCliente :: (PostBuild t m, DomBuilder t m) => Dynamic t Cliente -> m (Event t Acao)
tabCliente cl = do
  el "tr" $ do
    el "td" (dynText $ fmap clienteNome cl)
    el "td" (dynText $ fmap clienteTelefone cl)
    el "td" (dynText $ fmap clienteCpf cl)
    el "td" (dynText $ fmap clienteEndereco cl)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Perfil")
    let evt = (fmap (const PerfilCliente)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-warning" <> "style" =: "background: #ffc107; margin-left: 15px;") (text "Editar")
    let evt2 = (fmap (const EditarCliente)) (domEvent Click btn)
    return (attachPromptlyDynWith (flip ($)) (fmap codigoCliente cl) (leftmost [evt, evt2]))

reqClienteLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Workflow t m T.Text
reqClienteLista = Workflow $ do
  elAttr "div" ("class" =: "col-6 d-flex justify-content-center flex-column") $ do
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
          elAttr "th" ("scope" =: "col") (text "Endereco")
          elAttr "th" ("scope" =: "col") (text "")


      el "tbody" $ do
        simpleList dynCli tabCliente
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
      escolherPag (PerfilCliente pid) = pagPerfilCliente pid
      escolherPag (EditarCliente pid) = editarPerfilCliente pid

editarPerfilServico :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilServico pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  serv :: Dynamic t (Event t (Maybe Servicos)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- return (switchDyn serv)
  dynE <- return ((fromMaybe (Servicos 1 0 "" 0)) <$> mdyn)

  elAttr "div" ("class" =: "col-3 d-flex flex-column") $ do
    el "label" (text "Servico")
    servico <- inputElement $ def & inputElementConfig_setValue .~ (fmap servico dynE)
    el "label" (text "Valor")
    valor <-  numberInputDyn (fmap valor dynE)

    let servi = fmap(\(s,v) -> Servicos 1 0 s v) (zipDyn (_inputElement_value servico)  valor)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Editar")
    let submitBtn = domEvent Click btn
    let servEvt = tag (current servi) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$>
                performRequestAsync (sendRequest (BackendRoute_ServicosEditar :/ pid)
                <$> servEvt))
    return ("Editar: " <> (T.pack $ show pid), reqServicoLista <$ submitBtn)  


reqListaService :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqListaService = do
  r <- workflow reqServicoLista
  el "div" (dynText r)

pagPerfilServico :: ( DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilServico pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  prod :: Dynamic t (Event t (Maybe Servicos)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getServicosReq pid) <$> evt))
  mdyn <- holdDyn Nothing (switchDyn prod)
  dynServ <- return ((fromMaybe (Servicos 1 0 "" 0)) <$> mdyn)
  el "div" $ do
    elAttr "div" ("class" =: "d-flex flex-row align-items-center") $ do
      el "h5" (text "Servico:")
      el "h5" (dynText $ fmap servico dynServ)
    elAttr "div" ("class" =: "d-flex flex-row align-items-center") $ do
      el "h5" (text "Valor:")
      el "h5" (dynText $ fmap(T.pack . show . valor) dynServ)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger") (text "Voltar")
    let ret = domEvent Click btn
    return ("Perfil: " <> (T.pack $ show pid), reqServicoLista <$ ret)


reqServicos :: (DomBuilder t m, Prerender js t m) => m ()
reqServicos = do
  serv <- inputElement def
  vl <- numberInput
  let servi = fmap(\(s,v) -> Servicos 1 0 s v) (zipDyn (_inputElement_value serv)  vl)
  (submitBtn,_) <- el' "button" (text "Inserir")
  let click = domEvent Click submitBtn
  let servicosEvt = tag (current servi) click
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest(BackendRoute_Servicos :/ ()) <$> servicosEvt ))
  return()



tabServicos :: (PostBuild t m, DomBuilder t m) => Dynamic t Servicos -> m (Event t Acao)
tabServicos sr = do
  el "tr" $ do
    el "td" (dynText $ fmap servico sr)
    el "td" (dynText $ fmap (T.pack . show . valor) sr)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Perfil")
    let evt = (fmap (const PerfilServico)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-warning" <> "style" =: "background: #ffc107; margin-left: 15px;") (text "Editar")
    let evt2 = (fmap (const EditarServico)) (domEvent Click btn)
    return (attachPromptlyDynWith (flip ($)) (fmap codigoServico sr) (leftmost [evt, evt2]))

reqServicoLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Workflow t m T.Text
reqServicoLista = Workflow $ do
  (btn,_) <- el' "button" (text "Listar")
  let click = domEvent Click btn
  services :: Dynamic t (Event t (Maybe [Servicos])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getServiceListReq <$> click))
  evt <- return (fmap (fromMaybe []) $ switchDyn services)
  dynServ <- foldDyn (++) [] evt
  tb <- elAttr "table" ("class" =: "table") $ do
    el "thead" $ do
      el "tr" $ do
        elAttr "th" ("scope" =: "col") (text "Servico")
        elAttr "th" ("scope" =: "col") (text "valor")

    el "tbody" $ do
        simpleList dynServ tabServicos
  tb' <- return $ switchDyn $ fmap leftmost tb
  return ("Listagem", escolherPag <$> tb')
  where
    escolherPag (PerfilServico pid) = pagPerfilServico pid
    escolherPag (EditarServico pid) = editarPerfilServico pid


reqUsuario :: ( DomBuilder t m
        , Prerender js t m
        ) => m ()
reqUsuario = do
  elAttr "div" ("class" =: "d-flex flex-column col-4") $ do
    el "label" (text "Usuario") 
    username <- inputElement def
    el "label" (text "Senha") 
    senha <- inputElement def
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
  (btn,_) <- el' "button" (text "Listar")
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
    Usuarios -> reqUsuarioLista
    Clientes -> reqListaClient
    ServicosPag -> reqListaService
    InsertUser -> reqUsuario
    InsertCli -> reqCliente
    InsertServ -> reqServicos

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
  senha <- inputElement def
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
  evs <- el "ul" $ do
    elAttr "nav" ("class" =: "navbar navbar-expand-lg navbar-light bg-light") $ do
      elAttr "div" ("class" =: "container-fluid") $ do
        elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") (text "Navbar")
        elAttr "button" ("class" =: "navbar-toggler" <> "type" =: "button" <> "data-bs-toggle" =:"collapse" <> "data-bs-target" =: "#navbarSupportedContent" <> "aria-controls" =: "navbarSupportedContent" <> "aria-expanded" =: "false" <> "aria-label" =: "Toggle navigation" ) $ do
          elAttr "span" ("class" =: "navbar-toggle-icon") blank
        elAttr "div" ("class" =: "collapse navbar-collapse" <> "id" =: "navbarSupporedContent") $ do
          elAttr "ul" ("class" =: "navbar-nav me-auto mb-2 mb-lg-0") $ do
            p1 <- clickli Principal "Teste da Home"
            pu <- clickli Usuarios "Usuarios"
            p2 <- clickli Clientes "Clientes"
            p3 <- clickli ServicosPag "Servicos"
            p4 <- clickli InsertUser "Inserir Usuario"
            p5 <- clickli InsertCli "Inserir Cliente"
            p6 <- clickli InsertServ "Inserir Servicos"
            return (leftmost [p1,pu,p2,p3,p4,p5,p6])
  holdDyn Principal evs

numberInput :: (Read a, Num a) => DomBuilder t m => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n
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
      el "title" $ text "Obelisk Minimal Example"
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
