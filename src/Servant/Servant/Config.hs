{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Servant.Servant.Config where

import Data.Aeson
import Network.URL (URL)
import GHC.Generics (Generic)

import Servant.Servant.Types

data Config = Config
  { packages :: [Package]
  , port     :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


