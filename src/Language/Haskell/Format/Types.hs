{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.Format.Types
  ( CommentedSrc
  , takesOneLine
  ) where

import Language.Haskell.Exts

type CommentedSrc = (SrcSpanInfo, [Comment])

takesOneLine :: CommentedSrc -> Bool
takesOneLine (SrcSpanInfo { srcInfoSpan }, _) =
  case srcInfoSpan of
    SrcSpan { srcSpanStartLine, srcSpanEndLine } ->
      srcSpanStartLine == srcSpanEndLine
