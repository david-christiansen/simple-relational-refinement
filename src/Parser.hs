{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Lang

import Control.Lens hiding (List, op)
import Data.Char
import Data.Functor
import Data.Function
import Data.Text (Text)
import Data.Void

import Control.Monad.Combinators.Expr
import Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type LParser a = Parser (Located a)

skipWhitespace :: Parser ()
skipWhitespace = L.space space1 lineComment blockComment
  where lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser (Located a)
lexeme p = L.lexeme skipWhitespace (located p)

located :: Parser a -> Parser (Located a)
located p =
  do SourcePos _ sl sc <- getSourcePos
     let start = Pos (unPos sl) (unPos sc)
     res <- p
     SourcePos _ el ec <- getSourcePos
     let end = Pos (unPos el) (unPos ec)
     return $ Located (Span start end) res

kw :: Text -> Parser (Located ())
kw k =
  lexeme (string k <* notFollowedBy alphaNumChar) <&>
  fmap (const ())

op :: Text -> Parser (Located ())
op o =
  lexeme (string o <* notFollowedBy (oneOf ("*+-/" :: String))) <&>
  fmap (const ())

name :: Parser (Located Name)
name = lexeme (Name <$> ident)
  where
    ident = (:) <$> satisfy isLetter <*> many alphaNumChar

int :: Parser (Located Integer)
int = lexeme (read <$> (some (satisfy isDigit)))

bool :: LParser Bool
bool =
      fmap (const True) <$> (kw "true")
  <|> fmap (const False) <$> (kw "false")

baseType :: LParser BaseType
baseType =
       (kw "Unit" <&> fmap (const Unit))
   <|> (kw "Int" <&> fmap (const Int))
   <|> (kw "Bool" <&> fmap (const Bool))
   <|> (do Located l1 _ <- kw "List"
           ty@(Located l2 _) <- baseType
           return (Located (spanning l1 l2) (List ty)))

delim :: Char -> LParser ()
delim c = lexeme (char c $> ())

relSort :: LParser RelSort
relSort =
  do Located l1 _ <- delim '⟨'
     res <- RelSort <$> sepBy baseType (delim ',')
     Located l2 _ <- delim '⟩'
     return (Located (spanning l1 l2) res)

decl :: Parser (Located Decl)
decl = located $
      kw "relation" *>
      (RelDec <$> name <*> baseType <*> relSort)
  <|> kw "define" *>
      (Def <$> name <* delim '=' <*> synth)

data Expr :: * where
  EChk :: Located Check -> Expr
  ESyn :: Located Synth -> Expr
  deriving Show

baseTy :: LParser BaseType
baseTy = aBaseTy <|> aList
  where
    aBaseTy
      =  (kw "Int" <&> fmap (const Int))
     <|> (kw "Bool" <&> fmap (const Bool))
     <|> (kw "Unit" <&> fmap (const Unit))
     <|> delim '(' *> baseTy <* delim ')'
    aList
      = combine2 (\ _ t -> List t) <$> kw "List" <*> aBaseTy


ty :: LParser Ty
ty =
  do arg <- aType
     (((op "->" <|> op "→") *> ty) >>=
      pure . combine2 Arrow arg) <|> pure arg

aType :: LParser Ty
aType
   =  delim '(' *> ty <* delim ')'
  <|> refinement
  <|> promoteBase <$> baseTy
  where
    promoteBase bt@(Located l _) =
      Located l (RefTy (Located l NoName) bt (Located l Yep))


refinement :: LParser Ty
refinement =
  do Located l1 _ <- delim '{'
     x <- name
     delim ':'
     t@(Located tl _) <- baseTy
     φ <- optional (delim '|' *> predicate) >>=
          \case
            Nothing -> pure (Located tl Yep)
            Just φ -> pure φ
     Located l2 _ <- delim '}'
     pure (Located (spanning l1 l2) (RefTy x t φ))

predicate :: LParser Pred
predicate = fmap (const Yep) <$> kw "Yep" -- TODO more

combine2 :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
combine2 f x@(Located l1 _) y@(Located l2 _) =
  Located (spanning l1 l2) (f x y)


combine3 :: (Located a -> Located b -> Located c -> d) -> Located a -> Located b -> Located c -> Located d
combine3 f x@(Located l1 _) y z@(Located l2 _)=
  Located (spanning l1 l2) (f x y z)

check :: LParser Check
check = expr >>= getCheck

getCheck :: Expr -> LParser Check
getCheck (EChk c) =
  pure c
getCheck (ESyn s@(Located l _)) =
  pure (Located l (Synth s))


synth :: LParser Synth
synth =
  expr >>= getSynth

getSynth (EChk _) = fail $ "Missing type annotation"
getSynth (ESyn s) = pure s


atomic :: Parser Expr
atomic
   =  ESyn <$> (fmap IntLit <$> int)
  <|> ESyn <$> (fmap BoolLit <$> bool)
  <|> delim '(' *> expr <* delim ')'
  <|> EChk <$> (combine3 (const Lam) <$> (kw "fun" <|> kw "λ") <*> name <*> (delim '.' *> check))
  <|> EChk <$> (combine2 (const (const Nil)) <$> delim '[' <*> delim ']')
  <|>  ESyn <$> (fmap Var <$> name)
term :: Parser Expr
term =
  do fun <- atomic
     args <- many (atomic >>= getCheck)
     case args of
       [] -> return fun
       _  -> getSynth fun >>= flip combine args
  where
    combine :: Located Synth -> [Located Check] -> Parser Expr
    combine fun [] = pure (ESyn fun)
    combine fun (a:as) =
      combine (combine2 App fun a) as

factor :: Parser Expr
factor =
  term >>=
  \e1 ->
    (op "*" *> expr >>= mkBin Times e1) <|>
    pure e1

arith :: Parser Expr
arith =
  factor >>=
  \e1 ->
    (op "+" *> expr >>= mkBin Plus e1) <|>
    (op "-" *> expr >>= mkBin Minus e1) <|>
    pure e1

expr :: Parser Expr
expr =
  arith >>=
  \e ->
    (op ":" *>
     getCheck e >>= \ c ->
        ty >>=
        pure . ESyn . combine2 Annot c) <|> return e



mkBin :: BinOp -> Expr -> Expr -> Parser Expr
mkBin bin e1 e2 =
  ESyn <$> (combine2 (Bin bin) <$>
            getCheck e1 <*>
            getCheck e2)


prog :: Parser Prog
prog = Prog <$> sepBy decl (delim ';') <*> synth
