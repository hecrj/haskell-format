module Language.Haskell.Format.Types
  ( CommentedSrc
  ) where

import Language.Haskell.Exts

type CommentedSrc = (SrcSpanInfo, [Comment])
