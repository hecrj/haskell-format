module Language.Haskell.Format.Atom
  ( moduleName
  , name
  , cname
  ) where

import Language.Haskell.Exts hiding (name)
import Language.Haskell.Format.Internal as Format
import Language.Haskell.Format.Types

moduleName :: ModuleName CommentedSrc -> Format
moduleName (ModuleName _ name') = Format.fromString name'

name :: Name CommentedSrc -> Format
name (Ident _ name')  = Format.fromString name'
name (Symbol _ name') = Format.fromString name'

cname :: CName CommentedSrc -> Format
cname (VarName _ name') = name name'
cname (ConName _ name') = name name'
