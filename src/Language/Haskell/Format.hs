module Language.Haskell.Format
    ( file
    ) where

import Language.Haskell.Exts
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Module as Module

file :: FilePath -> IO String
file filepath = do
  result <- parseFileWithComments defaultParseMode filepath
  case result of
    ParseOk ast       -> return $ Format.toString (Module.format (associateHaddock ast))
    ParseFailed _ err -> return err
