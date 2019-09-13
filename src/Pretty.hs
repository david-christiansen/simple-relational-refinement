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
    group $ vsep [pretty t1, "→", pretty t2]
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
  pretty (Annot e t) = group $ vsep [pretty e <+> ":", pretty t]
  pretty (App fun arg) = parens $ align $ group $ vsep [pretty fun, pretty arg]
  pretty (Var x) = pretty x
  pretty (IntLit n) = pretty n
  pretty (BoolLit b) = pretty b
  pretty (Bin op a b) = parens $ align $ group $
                        vsep [pretty a <+> pretty op, pretty b]
  pretty UnitCon = "unit"

instance Pretty Check where
  pretty (Synth s) = pretty s
  pretty (Lam x e) = hang 2 $ group $ vsep ["λ" <> pretty x <> ".", pretty e]
  pretty Nil = "[]"
  pretty (Cons x xs) =
    hang 2 $ group $ vsep [pretty x <+> "::", pretty xs]
  pretty (RecList l b s) =
    "recList(" <> (align $ group $ vsep [ pretty l <> ";"
                                        , pretty b <> ";"
                                        , pretty s
                                        , ")"])
  pretty (If c t f) =
    "if(" <> (align $ group $ vsep [ pretty c <> ";"
                                   , pretty t <> ";"
                                   , pretty f
                                   , ")"
                                   ])

instance Pretty TypeError where
  pretty (GenericErr t) = pretty t
  pretty (UnknownVar x) = "Unknown var:" <+> pretty x
  pretty (NotAFunctionType t) =
    hang 2 $ group $ vsep [ "Not a function type:"
                          , pretty t
                          ]
  pretty (NotAListType t) =
    hang 2 $ group $ vsep [ "Not a list type:"
                          , pretty t
                          ]
  pretty (NotSubtype t1 t2) =
    hang 2 $ group $ vsep [ "Used a"
                          , pretty t1
                          , "where a"
                          , pretty t2
                          , "was expected."
                          ]
  pretty (BaseMismatch t1 t2) =
    pretty t1 <+> "≠" <+> pretty t2
  pretty Unsat = "Unsat"
