module Utils
    ( listSubdirectories
    , listFilesWithExtension
    , scanDirectory
    , TestFile(..)
    , TestDirectory(..)
    ) where

import           Control.Monad.Extra (filterM, partitionM)

import           Data.Foldable       (foldr')
import           Data.Functor
import           Data.List           (foldl')
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T

import           System.Directory
                 (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath
                 ((</>), stripExtension, takeExtension, takeFileName)

listDirectoryWithPrefix :: FilePath -> IO [FilePath]
listDirectoryWithPrefix path = fmap (path </>) <$> listDirectory path

listSubdirectories :: FilePath -> IO [FilePath]
listSubdirectories path =
    listDirectoryWithPrefix path >>= filterM doesDirectoryExist

listFilesWithExtension :: FilePath -> FilePath -> IO [FilePath]
listFilesWithExtension path ext = filter ((==) ext . takeExtension)
    <$> listDirectoryWithPrefix path

data TestFile where
    InvalidSubdirectory :: FilePath -> TestFile
    InvalidFile :: String -> TestFile
    MissingExpectedOutput :: FilePath -> TestFile
    HaskellFile :: FilePath -> TestFile
    HaskellFileWithInputs :: FilePath -> [FilePath] -> TestFile
    deriving stock (Eq, Show)

data TestDirectory = TestDirectory (Maybe ConfigFileName) [TestFile]
    deriving stock (Eq, Show)

scanDirectory :: FilePath -> IO [TestDirectory]
scanDirectory rootDir = do
    directories <- listSubdirectories rootDir
    putStrLn $ "GOT SUBDIRECTORIES: " <> show directories

    mapM scanTestDirectory directories

scanTestDirectory :: FilePath -> IO TestDirectory
scanTestDirectory dir = do
    -- We only support one level of subdirectory hierarchy, so make sure
    -- there are only regular files in here.
    (files, nonFiles)
        <- partitionM doesFileExist =<< listDirectoryWithPrefix dir
    putStrLn $ "GOT FILES: " ++ show files
    putStrLn $ "GOT NON FILES: " ++ show nonFiles

    let fileList = partitionFileList files
    putStrLn $ "FILE LIST: " ++ show fileList

    -- See @MapEntry@ documentation for details.
    let fileMap = computeFileMap $ flFiles fileList
    putStrLn $ "FILE MAP: " ++ show fileMap

    let testCases = fileMapToTestFiles fileMap
    putStrLn $ "TEST CASES: " ++ show testCases

    pure $ TestDirectory (flConfig fileList)
        $ fmap InvalidSubdirectory nonFiles
        <> fmap InvalidFile (flErrors fileList) <> testCases

newtype ConfigFileName = ConfigFileName FilePath
    deriving stock (Eq, Show)

-- | File list.
-- |
-- | For each regular file, we need to distinguish three cases:
-- | * It is the optional per-directory configuration file.
-- | * It has a ".hs" extension.
-- | * Everything else.
data FileList = FileList { flConfig :: Maybe ConfigFileName
                         , flFiles  :: [FilePath]
                         , flErrors :: [String]
                         }
    deriving stock (Eq, Show)

partitionFileList :: [FilePath] -> FileList
partitionFileList = foldl' func $ FileList Nothing mempty mempty
  where
    func iter entry
        | takeFileName entry == "floskell.yaml" =
            iter { flConfig = Just $ ConfigFileName entry }

    func iter (stripExtension ".hs" -> Just entry) =
        iter { flFiles = entry : flFiles iter }

    func iter entry =
        iter { flErrors = ("Invalid file name: " ++ entry) : flErrors iter }

-- | Test case file map.
-- |
-- | We optionally support one or more unformatted input files alongside
-- | their expected output.
-- |
-- | To do so, we scan each test directory for all files ending with ".hs",
-- | then strip all filename extensions from them and group by their stem.
-- |
-- | For instance, suppose we have the following files:
-- |
-- | - foo.hs
-- | - bar.hs
-- | - bar.one.hs
-- | - bar.two.hs
-- | - baz.what.hs
-- |
-- | This gives us three stems: "foo", "bar" and "baz" - each represented
-- | by a @MapEntry@.
-- |
-- | Since there is only a single "foo.hs", it will get a `MapEntry True []`.
-- |
-- | For "bar", there is the expected result "bar.hs" and two input files, so
-- | we create `MapEntry True ["one", "two"]`.
-- |
-- | It is an error for "baz" not to have an expected results file, and we
-- | record this as `MapEntry False ["what"]`.
data MapEntry = MapEntry Bool [Text]
    deriving stock (Eq, Show)

instance Semigroup MapEntry where
    MapEntry leftHasMain leftExtras <> MapEntry rightHasMain rightExtras =
        MapEntry (leftHasMain || rightHasMain) (leftExtras <> rightExtras)

instance Monoid MapEntry where
    mempty = MapEntry False mempty

type FileMap = M.Map Text MapEntry

computeFileMap :: [FilePath] -> FileMap
computeFileMap = flip foldr' mempty $ func . T.breakOn "." . T.pack
  where
    func :: (Text, Text) -> FileMap -> FileMap
    func (stem, "") = M.insertWith (<>) stem (MapEntry True [])
    func (stem, exts) = M.insertWith (<>) stem $ MapEntry False [exts]

fileMapToTestFiles :: FileMap -> [TestFile]
fileMapToTestFiles fileMap =
    flip fmap (M.assocs fileMap) $ \(T.unpack -> key, value) -> case value of
        MapEntry True [] -> HaskellFile $ toKeyPath key
        MapEntry True inputs -> HaskellFileWithInputs (toKeyPath key)
            $ inputs <&> toInputPath key
        MapEntry False _ -> MissingExpectedOutput $ toKeyPath key
  where
    toKeyPath key = key ++ ".hs"

    toInputPath key input = key ++ T.unpack input ++ ".hs"
