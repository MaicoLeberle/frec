module ShowEntries ( showEntriesTrad ) where

import System.Directory ( doesDirectoryExist
                        , getDirectoryContents
                        )
import System.FilePath  ((</>))
import Control.Monad    (forM)


showEntriesTrad :: FilePath -> IO [(FilePath, [FilePath])]
showEntriesTrad path = do
    contents <- filter notDots <$> getDirectoryContents path
    rest <- concat <$> forM contents processEntryInDir
    return $ (path, contents) : rest
  where
    notDots :: FilePath -> Bool
    notDots p = p /= "." && p /= ".."

    processEntryInDir :: FilePath -> IO [(FilePath, [String])]
    processEntryInDir name = do
        let newName = path </> name
        isDir <- doesDirectoryExist newName
        if isDir then showEntriesTrad newName else return []
