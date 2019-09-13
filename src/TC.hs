{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TC where

import Control.Exception hiding (TypeError(..))
import Control.Lens hiding (Context, List)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Lang

data TypeError
  = GenericErr Text
  | UnknownVar (Located Name)
  | NotAFunctionType (Located Ty)
  deriving Show

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

tc :: TC a -> Context -> IO (Either TypeError a)
tc act ctx = (Right <$> runReaderT (runTC act) ctx) `catch` handler
  where
    handler :: TypeError -> IO (Either TypeError b)
    handler = pure . Left

io :: IO a -> TC a
io = TC . liftIO

failure :: TypeError -> TC a
failure e = io $ throwIO e

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
       Nothing -> failure (UnknownVar x)
       Just ty -> return ty

isPred :: Located Pred -> TC ()
isPred φ = located φ isPred'

isPred' Yep = pure ()
isPred' _ = failure $ GenericErr "Not a valid predicate"

isType :: Located Ty -> TC ()
isType t = located t isType'

isType' :: Ty -> TC ()
isType' (Arrow dom ran) = isType dom *> isType ran
isType' (RefTy x bt φ) =
   --  Γ, x : {_ : τ | ⊤} ; Σ ⊢ φ pred
   -- --------------------------------
   --  Γ;Σ ⊢ {x : τ | φ } type
  withExtendedContext (view value x) (view value $ liftBase bt) $
    isPred φ

check :: Ty -> Located Check  -> TC ()
check t c = located c (check' t)

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
         flip check a =<< ty Int
         ty Int
    Times ->
      do flip check a =<< ty Int
         flip check a =<< ty Int
         ty Int
    Minus ->
      do flip check a =<< ty Int
         flip check a =<< ty Int
         ty Int
    LTE ->
      do flip check a =<< ty Int
         flip check a =<< ty Int
         ty Bool
synth' UnitCon = ty Unit

isFunction :: Ty -> TC (Located Ty, Located Ty)
isFunction (Arrow dom ran) = return (dom, ran)
isFunction other = here other >>= failure . NotAFunctionType

here :: a -> TC (Located a)
here x =
  do ctx <- context
     return $ Located (view currentLocation ctx) x

ty :: BaseType -> TC Ty
ty bt =
  view value . liftBase <$> here bt
