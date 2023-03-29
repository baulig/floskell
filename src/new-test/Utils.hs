-- |
module Utils ( listSubdirectories ) where

import           Control.Monad    ( filterM )

import           System.Directory ( doesDirectoryExist, listDirectory )
import           System.FilePath  ( (</>) )

listSubdirectories :: FilePath -> IO [FilePath]
listSubdirectories path = listDirectory path
    >>= filterM (doesDirectoryExist . (path </>))
