module ShowEntries (showEntriesTrad, Name) where

import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)

type Name = String

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
	where notDots p = p /= "." && p /= ".."

showEntriesTrad :: FilePath -> IO [(FilePath, [Name])]
showEntriesTrad path = do
	contents <- listDirectory path
	rest <- forM contents $ (\name -> (do
			let newName = path </> name
		        isDir <- doesDirectoryExist newName
		        if isDir 
			then showEntriesTrad newName
			else return []))
        return $ (path, contents) : (concat rest)
