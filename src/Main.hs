import ShowEntries (showEntriesTrad)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import Control.Monad (mapM_, unless)
import Data.List (isInfixOf)

{-| Usage:

        program (search_starting_from) file_or_dir_to_find

    where skipping search_starting_from means start searching from PWD.
-}


main :: IO ()
main = do (path, objective) <- parseArgs <$> getArgs
          isDir <- doesDirectoryExist path
          unless isDir (error "From-path does not exist.")
          findMatchings path objective >>= mapM_ putStrLn

parseArgs :: [String] -> (FilePath, FilePath)
parseArgs args | length args == 1 = (".", head args)
               | length args == 2 = (head args, head $ tail args)
               |        otherwise = error "Usage: path file (or just file)"

findMatchings :: FilePath -> FilePath -> IO [FilePath]
findMatchings path name =
    showEntriesTrad path
        >>= return . (filter (name `isInfixOf`)) . concat . map snd
