module Test.HClTest.Setup 
  ( copyFiles
  , copyFilesHere
  ) where

import Control.Monad (foldM)
import Data.Foldable (for_)
import System.Directory
import System.FilePath

partitionM   :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f = flip foldM ([],[]) $ \(a,b) x -> do
  v <- f x
  return $ if v then (x:a,b) else (a,x:b)

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles source target = do
  (files,dirs) <- partitionM (doesFileExist . (source </>)) . filter (not . (`elem` [".",".."])) =<< getDirectoryContents source
  for_ files $ \file -> copyFile (source </> file) $ target </> file
  for_ dirs $ \dir  -> do
    createDirectory $ target </> dir
    copyFiles (source </> dir) $ target </> dir

copyFilesHere :: FilePath -> IO ()
copyFilesHere = flip copyFiles "."
