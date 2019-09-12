{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Lang

import Control.Lens hiding (List)
import Data.Char
import Data.Functor
import Data.Function
import Data.Text (Text)
import Data.Void

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

check :: LParser Check
check = _

ty :: LParser Ty
ty = _

combine2 :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
combine2 f x@(Located l1 _) y@(Located l2 _) =
  Located (spanning l1 l2) (f x y)

synth :: Parser (Located Synth)
synth
   =  combine2 Annot <$> check <* delim ':' <*> ty
  <|> combine2 App <$> synth <*> check
  <|> fmap Var <$> name
  <|> fmap IntLit <$> int
  <|> fmap BoolLit <$> bool
  


prog :: Parser Prog
prog = Prog <$> sepBy decl (delim ';') <*> synth
