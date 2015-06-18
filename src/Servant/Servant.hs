{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Servant.Servant where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API

import Servant.Servant.Types


type PackageAPI
    = Capture "package name" Text :> ( "build" :> BuildAPI
                                  :<|> Get '[JSON] Package
                                     )

type BuildAPI = Capture "build-id" Text                 :> Get '[JSON] BuildData
           :<|> "branches"  :> ReqBody '[JSON] [String] :> Put '[] ()
           :<|>                                            Post '[JSON] Package

type TheAPI = "package" :> PackageAPI
