-- |
module Main ( main ) where

import           Control.Monad

import           Data.Bool
import qualified Data.ByteString.Lazy  as BS

import           Floskell
import           Floskell.Styles

import           Language.Haskell.Exts ( Language(..) )

import           System.Directory
                 ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath       ( (</>), takeExtension )

import           Test.Hspec

import Utils

-- | Root directory of the test case tree.
rootDir :: FilePath
rootDir = "src/new-test/test-cases"

main :: IO ()
main = do
    entries <- scanDirectory rootDir
    putStrLn $ "GOT ENTRIES: " <> show entries

    directories <- listSubdirectories rootDir
    putStrLn $ "TEST: " <> show directories

    hspec $ do
        mapM_ directorySpec directories

-- | Create Hspec 'Spec' for test directory 'dir'.
directorySpec :: FilePath -> Spec
directorySpec dir = describe dir $ do
    testCases <- runIO $ listFilesWithExtension dir ".hs"
    it "contains any test cases." $ do
        length testCases `shouldNotBe` 0

    (custom, config) <- runIO $ readDirectoryConfig dir
    describe ("using " ++ bool "default" "custom" custom ++ " config") $
        mapM_ (fileSpec config) testCases

-- | Create Hspec 'Spec' for test case 'path', using configuration 'config'.
fileSpec :: AppConfig -> FilePath -> Spec
fileSpec config path = describe path $ do
    it "formats as expected." $ do
        code <- BS.readFile path
        case reformatSnippet config path code of
            Left e -> error e
            Right result -> result `shouldBe` code

reformatSnippet
    :: AppConfig -> FilePath -> BS.ByteString -> Either String BS.ByteString
reformatSnippet config path = reformat config (Just path)

readDirectoryConfig :: FilePath -> IO (Bool, AppConfig)
readDirectoryConfig dir = do
    hasConfig <- doesFileExist configFile
    (,) hasConfig <$> maybeReadConfig hasConfig
  where
    configFile = dir </> "floskell.yaml"

    maybeReadConfig True = readAppConfig configFile
    maybeReadConfig False =
        pure $ AppConfig martin Haskell2010 defaultExtensions mempty
