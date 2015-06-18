{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.Servant.Types where

import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.URL (URL(..), URLType(..), Host(..), Protocol(..), importURL)

import Servant.Servant.Utils

data Package = Package
  { packageName          :: String
  , packageRepo          :: GithubURL
  , packageRelPath       :: FilePath
  -- ^ dir of cabal file
  , packageWatchBranches :: [String]
  , packageOpenPRs       :: Maybe [Int]
  } deriving (Eq, Show, Typeable, Generic, ToJSON, FromJSON)

newtype GithubURL = GithubURL { unGH :: URL }
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON GithubURL where
  toJSON (GithubURL url) = String . pack $ show url

instance FromJSON GithubURL where
  parseJSON (String s) = case importURL $ unpack s of
    -- TODO: more complete validation
    Nothing  -> mzero
    Just url -> return $! GithubURL url
  parseJSON _          = mzero

data BuildData = BuildData
  { buildStatus :: Text
  , buildInfo   :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriveSafeCopy 0 'base ''Protocol
deriveSafeCopy 0 'base ''Host
deriveSafeCopy 0 'base ''URLType
deriveSafeCopy 0 'base ''URL
deriveSafeCopy 0 'base ''GithubURL
deriveSafeCopy 0 'base ''Package

parseGithubURL :: String -> Maybe GithubURL
parseGithubURL = undefined

repoName :: GithubURL -> String
repoName (GithubURL URL{..}) = last url_path
  where
    last = reverse . takeWhile (/= '/') . reverse
