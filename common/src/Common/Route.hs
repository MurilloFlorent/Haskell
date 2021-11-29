{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text, unpack)
import Data.Functor.Identity
import Data.Function
import Obelisk.Route
import Obelisk.Route.TH


checFullREnc :: Encoder Identity Identity(R (FullRoute BackendRoute FrontendRoute))PageName
checFullREnc = checkEncoder fullRouteEncoder &
  \case
    Left err -> error $ unpack err
    Right encoder -> encoder

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
 
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Usuario :: BackendRoute ()
  BackendRoute_Cliente :: BackendRoute ()
  BackendRoute_ClienteListar :: BackendRoute ()
  BackendRoute_Servicos :: BackendRoute ()
  BackendRoute_Agendamento :: BackendRoute ()
  
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Usuario -> PathSegment "usuario" $ unitEncoder mempty
      BackendRoute_Servicos -> PathSegment "servicos" $ unitEncoder mempty
      BackendRoute_Agendamento -> PathSegment "agendamento" $ unitEncoder mempty
      BackendRoute_ClienteListar -> PathSegment "clienteListar" $ unitEncoder mempty
      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
