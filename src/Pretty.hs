{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Pretty where


import Lang

import Control.Lens hiding (List)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

instance Pretty Name where
  pretty (Name x) = pretty x
  pretty NoName = pretty ("_" :: Text)

instance Pretty BaseType where
  pretty (List (view value -> List t)) =
    group $ vsep ["List", parens (pretty (List t))]
  pretty (List t) =
    group $ vsep ["List", pretty t]
  pretty Int = "Int"
  pretty Bool = "Bool"
  pretty Unit = "Unit"

instance Pretty BinOp where
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Times = "*"
  pretty LTE = "≤"

instance Pretty a => Pretty (Located a) where
  pretty = pretty . view value

instance Pretty Ty where
  pretty (Arrow (view value -> t1@(Arrow _ _)) t2) =
    group $ vsep [parens (pretty t1), "→", pretty t2]
  pretty (Arrow t1 t2) =
    group $ vsep [parens (pretty t1), "→", pretty t2]
  pretty (RefTy (view value -> NoName) (view value -> bt) _) = pretty bt
  pretty (RefTy x t (view value -> Yep)) =
    braces $ pretty x <+> ":" <+> align (pretty t)
  pretty (RefTy x t φ) =
    braces $ pretty x <+> align (group (vsep  [ ":" <+> align (pretty t)
                                              , "|" <+> align (pretty φ)
                                              ]))
instance Pretty Pred where
  pretty Yep = "Yep"
  pretty _ = error "not implemented"

instance Pretty Synth where
  pretty _ = "synth"

instance Pretty Check where
  pretty _ = "chk"