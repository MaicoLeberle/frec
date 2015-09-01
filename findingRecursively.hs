import ShowEntries (showEntriesTrad, Name)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import Control.Monad (mapM_)
import Data.List (isInfixOf)

main = do  
	--  Usage: program search_starting_from file_or_dir_to_find
        -- 	or program file_or_dir_to_find (searches from ./)
	args <- getArgs
	let (path, objective) = parseArgs args
	isDir <- doesDirectoryExist path
	if isDir
	then do
		results <- findMatchings path objective
		mapM_ putStrLn results
	else error "From-path does not exist."


parseArgs :: [String] -> (FilePath, Name)
parseArgs args = if n /= 1 && n /= 2
	         then error "Usage: path file (or just file)"
		 else if n == 1
  		      then (".", head args)
		      else (head args, head $ tail args)
	where n = length args

findMatchings :: FilePath -> Name -> IO [FilePath]
findMatchings path name = do
	r <- showEntriesTrad path
	--	mapM_ (putStrLn . show) r
	return (filter (name `isInfixOf`) $ concat $ map snd r)
	
