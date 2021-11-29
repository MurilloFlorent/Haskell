{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Control.Monad

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Data.Aeson


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.


data Pagina = Principal | Categoria | Produtos | Pagina4
getPath :: R BackendRoute ->  T.Text
getPath r = renderBackendRoute checFullREnc r

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

reqUsuario :: ( DomBuilder t m
        , Prerender js t m
        ) => m ()
reqUsuario = do
  username <- inputElement def
  senha <- inputElement def
  let user = fmap (\(u,s) -> Usuario 0 u s) (zipDyn (_inputElement_value username)(_inputElement_value  senha))
  (submitBtn,_) <- el' "button" (text "Inserir")
  let click = domEvent Click submitBtn
  let userEvt = tag (current user) click
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Usuario :/ ()) <$> userEvt ))
  return ()

clickli :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickli p t = do
  (ev, _) <- elAttr' "li" ("class" =: "nav-item") (elAttr "a" ("href" =: "#" <> "class" =: "nav-link") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender js t m) => Pagina -> m ()
currPag p = 
  case p of
    Principal -> blank
    Categoria -> catpag
    Produtos -> prodpag
    Pagina4 -> reqUsuario

homepag :: (DomBuilder t m , PostBuild t m, MonadHold t m, Prerender js t m) => m ()
homepag = do
  pagina <- el "div" menu
  dyn_ $ currPag <$> pagina
  login

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
            p2 <- clickli Categoria "Categoria"
            p3 <- clickli Produtos "Produtos"
            p4 <- clickli Pagina4 "Insercao banco"
            return (leftmost [p1,p2,p3,p4])
  holdDyn Principal evs


      



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
  , _frontend_body = homepag
  }
