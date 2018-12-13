module Main where

import System.Environment
import System.Exit
import System.FilePath ((</>))
import qualified Data.List as List
import qualified Language.Haskell.Format as Format
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO


main :: IO ()
main =
    getArgs >>= format


format :: [String] -> IO ()
format args =
    case args of
        path:_ ->
            do
                isDirectory <- Directory.doesDirectoryExist path

                if isDirectory then
                    do
                        haskellFiles <- findHaskellFiles path
                        confirmation <- askForConfirmation haskellFiles

                        if confirmation then
                            mapM_ formatAndOverwriteFile haskellFiles

                        else
                            exitWith (ExitFailure 1)

                else
                    do
                        tryFormat <- Format.file path

                        case tryFormat of
                            Left err ->
                                error err

                            Right format ->
                                putStr format

        _ ->
            error "no file provided"


findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles path = do
    isDirectory <- Directory.doesDirectoryExist path

    if isDirectory then
        do
            contents <- Directory.listDirectory path

            mconcat
                <$> mapM
                    (findHaskellFiles . (path </>))
                    (filter (not . List.isPrefixOf ".") contents)

    else
        if FilePath.takeExtension path == ".hs" then
            return [ path ]

        else
            return []


askForConfirmation :: [FilePath] -> IO Bool
askForConfirmation files = do
    putStrLn "This will format and overwrite the following files:"
    mapM_ (putStrLn . ("    " ++)) files
    putStr "Are you sure? [y/N] "
    IO.hFlush IO.stdout
    confirmation <- getLine
    return (confirmation == "y")


formatAndOverwriteFile :: FilePath -> IO ()
formatAndOverwriteFile file = do
    tryFormat <- Format.file file

    case tryFormat of
        Left _ ->
            putStrLn $ "    " ++ file ++ " ERROR"

        Right format ->
            do
                writeFile file format
                putStrLn $ "    " ++ file ++ " OK"
