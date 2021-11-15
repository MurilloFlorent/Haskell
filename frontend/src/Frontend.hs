{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
data Pagina = Principal | Categoria | Produtos

clickli :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickli p t = do
  (ev, _) <- elAttr' "li" ("class" =: "nav-item") (elAttr "a" ("href" =: "#" <> "class" =: "nav-link") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> m ()
currPag p = 
  case p of
    Principal -> blank
    Categoria -> catpag
    Produtos -> prodpag

homepag :: (DomBuilder t m , PostBuild t m, MonadHold t m) => m ()
homepag = do
  pagina <- el "div" menu
  el "ul" $ do
    el "li" (text "Murillo Florentino")
    el "li" (text "Rebeka Alexandra")
    el "li" (text "Bruno Fontes")
    el "li" (text "Rodrigo Hailer")
  dyn_ $ currPag <$> pagina

catpag :: (DomBuilder t m , PostBuild t m, MonadHold t m) => m ()
catpag = do
  el "h1" (text "Pagina Categorias")


prodpag :: (DomBuilder t m , PostBuild t m, MonadHold t m) => m ()
prodpag = do
  el "h1" (text "Pagina Produtos")


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
            return (leftmost [p1,p2,p3])
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
