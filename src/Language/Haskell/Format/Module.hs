module Language.Haskell.Format.Module
  ( full
  ) where

import Language.Haskell.Exts hiding (name)
import Prelude hiding (head)

full :: (Module SrcSpanInfo, [Comment]) -> String
full (Module info head' pragmas imports declarations, comments) =
  head head'

head :: Maybe (ModuleHead SrcSpanInfo) -> String
head Nothing = ""
head (Just (ModuleHead _ name' _ exports)) =
  "module " ++ name name' ++ " where\n"

name :: ModuleName SrcSpanInfo -> String
name (ModuleName _ name') = name'
