{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Servant.Servant.Db
  ( PackageDB(..)
  , AddPackageIfMissing
  , GetPackage
  ) where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Acid
import Data.SafeCopy
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Servant.Servant.Types


newtype PackageDB = PackageDB { unPkgDB :: Map.Map String Package }
  deriving (Typeable, Generic)

deriveSafeCopy 0 'base ''PackageDB

addPackageIfMissing :: Package -> Update PackageDB ()
addPackageIfMissing pkg = modify go
  where
    pickOld _ old = old
    go (PackageDB db) = PackageDB
      $ Map.insertWith pickOld (packageName pkg) pkg db

getPackage :: String -> Query PackageDB (Maybe Package)
getPackage pkgName = ask >>= return . Map.lookup pkgName . unPkgDB

makeAcidic ''PackageDB ['addPackageIfMissing, 'getPackage]
