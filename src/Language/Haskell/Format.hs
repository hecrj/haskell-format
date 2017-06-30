module Language.Haskell.Format
    ( file
    ) where

import Data.ByteString
import Language.Haskell.Exts
import Language.Haskell.Format.Module as Module

file :: FilePath -> IO String
file filepath = do
  result <- parseFileWithComments defaultParseMode filepath
  case result of
    ParseOk ast       -> return $ Module.full ast
    ParseFailed _ err -> return err