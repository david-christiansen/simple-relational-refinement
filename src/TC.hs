{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module TC where

import Control.Exception hiding (TypeError(..))
import Control.Lens hiding (Context, List)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Lang

instance Exception a => Exception (Located a) where

instance Exception TypeError where

data Context = Context
  { _varTypes :: Map Name Ty
  , _relationDefs :: Map Name ()
  , _currentLocation :: Span
  }

emptyContext span = Context mempty mempty span

varTypes :: Simple Lens Context (Map Name Ty)
varTypes = lens _varTypes (\ctx tys -> ctx { _varTypes = tys })

relationDefs :: Simple Lens Context (Map Name ())
relationDefs = lens _relationDefs (\ctx rs -> ctx { _relationDefs = rs })

currentLocation :: Simple Lens Context Span
currentLocation = lens _currentLocation (\ctx l -> ctx {_currentLocation = l})

newtype TC a = TC { runTC :: ReaderT Context IO a }
  deriving (Functor, Applicative, Monad)

tc :: TC a -> Context -> IO (Either (Located TypeError) a)
tc act ctx = (Right <$> runReaderT (runTC act) ctx) `catch` handler
  where
    handler :: Located TypeError -> IO (Either (Located TypeError) b)
    handler = pure . Left

io :: IO a -> TC a
io = TC . liftIO

failure :: TypeError -> TC a
failure e = here e >>= io . throwIO

context :: TC Context
context = TC ask

withExtendedContext :: Name -> Ty -> TC a -> TC a
withExtendedContext x t act =
  TC (local (over varTypes (Map.insert x t)) (runTC act))

located :: Located a -> (a -> TC b) -> TC b
located v f =
  TC (local (set currentLocation (view location v)) (runTC $ f (view value v)))

lookupVar :: Located Name -> TC Ty
lookupVar x =
  do t <- view (varTypes . at (view value x)) <$> context
     case t of
       Nothing -> failure (UnknownVar (view value x))
       Just ty -> return ty

isPred :: Located Pred -> TC ()
isPred φ = located φ isPred'

isPred' Yep = pure ()
isPred' _ = failure $ GenericErr "Not a valid predicate"

isType :: Located Ty -> TC ()
isType t = located t isType'

isType' :: Ty -> TC ()
isType' (Arrow dom ran) =
  isType dom *>
  isType ran
isType' (RefTy x bt φ) =
  --  Γ, x : {_ : τ | ⊤} ; Σ ⊢ φ pred
  -- --------------------------------
  --  Γ;Σ ⊢ {x : τ | φ } type
  withExtendedContext (view value x) (view value $ liftBase bt) $
    isPred φ

isTrue :: Pred -> TC ()
isTrue Yep = return ()
isTrue Nope = failure Unsat
isTrue other = failure $ GenericErr "Truth not yet implemented"

sameBaseType :: BaseType -> BaseType -> TC ()
sameBaseType Int Int = return ()
sameBaseType Bool Bool = return ()
sameBaseType Unit Unit = return ()
sameBaseType (List t1) (List t2) = sameBaseType (view value t1) (view value t2)
sameBaseType bt1 bt2 = failure $ BaseMismatch bt1 bt2

neg :: Pred -> Pred
neg Yep = Nope
neg Nope = Yep
neg (And φ ψ) = disj (neg φ) (neg ψ)
neg (Or φ ψ) = conj (neg φ) (neg ψ)
neg (Not φ) = neg (neg φ)
neg φ = Not φ

disj :: Pred -> Pred -> Pred
disj Yep φ = Yep
disj φ Yep = Yep
disj Nope φ = φ
disj φ Nope = φ
disj φ ψ = Or φ ψ

conj :: Pred -> Pred -> Pred
conj Yep φ = φ
conj φ Yep = φ
conj Nope _ = Nope
conj _ Nope = Nope
conj φ ψ = And φ ψ

isSubtype :: Ty -> Ty -> TC ()
isSubtype (Arrow t1 t2) (Arrow t1' t2') =
  do isSubtype (view value t1') (view value t1)
     isSubtype (view value t2) (view value t2')
isSubtype (RefTy x (view value -> bt) φ) (RefTy y (view value -> bt') φ') =
  do sameBaseType bt bt'
     isTrue $ neg (view value φ) `disj` (view value φ')
  -- τ = τ'    Γ;Σ ⊨ φ ⊃ φ'
  -- ----------------------------------
  -- Γ;Σ ⊢ {x : τ | φ} :< {y : τ' | φ'}
isSubtype other1 other2 =
  failure $ NotSubtype other1 other2

check :: Ty -> Located Check  -> TC ()
check t c = located c (check' t)

check' t (Synth s) =
  do t' <- synth s
     isSubtype t' t
check' t (Lam x body) =
  do (dom, ran) <- isFunction t
     withExtendedContext (view value x) (view value dom) $
       check (view value ran) body
check' t Nil =
  isList t *>
  return () -- TODO check refinement
check' t c = failure $ GenericErr "can't check yet!!!"

synth :: Located Synth -> TC Ty
synth s = located s synth'

synth' :: Synth -> TC Ty
synth' (Annot c t) =
  do isType t
     check (view value t) c
     return (view value t)
synth' (App fun arg) =
  do funT <- synth fun
     (dom, ran) <- isFunction funT
     check (view value dom) arg
     return (view value ran) -- FIXME instantiate the variable
synth' (Var x) = lookupVar =<< here x
synth' (IntLit n) = ty Int
synth' (BoolLit b) = ty Bool
synth' (Bin op a b) =
  case op of
    Plus ->
      do flip check a =<< ty Int
         flip check b =<< ty Int
         ty Int
    Times ->
      do flip check a =<< ty Int
         flip check b =<< ty Int
         ty Int
    Minus ->
      do flip check a =<< ty Int
         flip check b =<< ty Int
         ty Int
    LTE ->
      do flip check a =<< ty Int
         flip check b =<< ty Int
         ty Bool
synth' UnitCon = ty Unit

isFunction :: Ty -> TC (Located Ty, Located Ty)
isFunction (Arrow dom ran) = return (dom, ran)
isFunction other = failure (NotAFunctionType other)

isList :: Ty -> TC (Located BaseType, Pred)
isList (RefTy _ (view value -> (List a)) (view value -> φ)) =
  return (a, φ)
isList other =
  failure (NotAListType other)


here :: a -> TC (Located a)
here x =
  do ctx <- context
     return $ Located (view currentLocation ctx) x

ty :: BaseType -> TC Ty
ty bt =
  view value . liftBase <$> here bt
