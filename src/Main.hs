{-# LANGUAGE DeriveFunctor #-}
module Main where

import Lang
import Parser
import Pretty

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import System.IO
import Text.Megaparsec

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     putStr "> "
     l <- getLine
     let parseRes = parse expr "⟨repl⟩" (T.pack l)
     case parseRes of
       Left err -> putStrLn (errorBundlePretty err) *> main
       Right (ESyn e) -> print (pretty e) *> main
       Right (EChk e) -> print (pretty e) *> main
