{-# LANGUAGE RecordWildCards #-}
module Servant.Servant.Filestore where

import Data.FileStore (create)
import Data.FileStore.Git (gitFileStore)
import Servant.Servant.Types

startRepo :: GithubURL -> IO ()

commit :: FilePath -> IO ()


