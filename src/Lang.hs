{-# LANGUAGE DeriveFunctor #-}
module Lang where

import Control.Lens
import Data.Text (Text)

data Pos = Pos
  { _line :: Int
  , _col :: Int
  }
  deriving Show

line :: Simple Lens Pos Int
line = lens _line (\p l -> p {_line = l})

col :: Simple Lens Pos Int
col = lens _col (\p c -> p {_col = c})

data Span = Span
  { _start :: Pos
  , _end :: Pos
  }
  deriving Show

start :: Simple Lens Span Pos
start = lens _start (\s b -> s {_start = b})

end :: Simple Lens Span Pos
end = lens _end (\s e -> s {_end = e})

spanning :: Span -> Span -> Span
spanning (Span s _) (Span _ e) = Span s e

data Located a = Located
  { _location :: Span
  , _value :: a
  }
  deriving (Functor, Show)

location :: Simple Lens (Located a) Span
location = lens _location (\l s -> l {_location = s})

value :: Lens (Located a) (Located b) a b
value = lens _value (\l v -> l {_value = v})

data Name = Name String
          | NoName
  deriving (Eq, Ord, Show)

data BaseType
  = Int
  | Bool
  | Unit
  | List (Located BaseType)
  deriving (Show)


newtype RelSort = RelSort [Located BaseType]
  deriving Show

data BinOp = Plus | Minus | Times | LTE
  deriving Show

data Ty
  = Arrow (Located Ty) (Located Ty)
  | RefTy (Located Name) (Located BaseType) (Located Pred)
  deriving Show

liftBase :: Located BaseType -> Located Ty
liftBase τ = set value (RefTy (set value NoName τ) τ (set value Yep τ)) τ

data Synth
  = Annot (Located Check) (Located Ty)
  | App (Located Synth) (Located Check)
  | Var Name
  | IntLit Integer
  | BoolLit Bool
  | Bin BinOp (Located Check) (Located Check)
  | UnitCon
  deriving Show

data Check
  = Synth (Located Synth)
  | Lam (Located Name) (Located Check)
  | Nil
  | Cons (Located Check) (Located Check)
  | RecList (Located Synth) (Located Check) (Located Check)
  | If (Located Check) (Located Check) (Located Check)
  deriving Show

data Star = Star
  deriving Show

data Rel
  = RApp (Located Name) (Maybe (Located Star)) (Located Check)
  | Prod Rel Rel
  | Union Rel Rel
  | LitRel [[Located Synth]]
  deriving (Show)


data Pred
  = Eq Rel Rel
  | SubsetEq Rel Rel
  | And Pred Pred
  | Or Pred Pred
  | Not Pred
  | Yep | Nope
  deriving Show

data RelDef
  = ListRel (Located Rel) (Located Name, Located Name, Located Rel)
  | BoolRel (Located Rel) (Located Rel)
  | OtherRel (Located Name) (Located Rel)
  deriving Show

data Decl
  = RelDec (Located Name) (Located BaseType) (Located RelSort) (Located RelDef)
  | Def (Located Name) (Located Synth)
  deriving Show

data Prog = Prog
  { _declarations :: [Located Decl]
  , _body :: Located Synth
  }
  deriving Show

declarations :: Simple Lens Prog [Located Decl]
declarations = lens _declarations (\p ds -> p {_declarations = ds})

body :: Simple Lens Prog (Located Synth)
body = lens _body (\p b -> p {_body = b})

data TypeError
  = GenericErr Text
  | UnknownVar Name
  | NotAFunctionType Ty
  | NotAListType Ty
  | NotSubtype Ty Ty
  | BaseMismatch BaseType BaseType
  | RelationDefined Name
  | Unsat -- TODO insert reason why
  deriving Show
