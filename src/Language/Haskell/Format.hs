module Language.Haskell.Format
    ( file
    ) where

import Language.Haskell.Exts
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Module as Module
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception


file :: FilePath -> IO (Either String String)
file filepath = do
    result <- parseFileWithComments defaultParseMode filepath

    case result of
        ParseOk ast -> do
            tryFormat <-
                Exception.try
                    (Exception.evaluate $
                        DeepSeq.force $
                            Format.toString $
                                Module.format (associateHaddock ast)
                    )

            case tryFormat of
                Left err@(Exception.ErrorCallWithLocation _ _) ->
                    return $ Left (show err)

                Right format ->
                    return $ Right format

        ParseFailed _ err ->
            return $ Left err
