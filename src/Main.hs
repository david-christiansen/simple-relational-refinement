{-# LANGUAGE DeriveFunctor #-}
module Main where

import Lang
import Parser
import Pretty

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import System.IO
import System.Console.Haskeline
import Text.Megaparsec

main :: IO ()
main = runInputT defaultSettings repl
  where
    repl =
      do l <- getInputLine "> "
         case l of
           Nothing -> return ()
           Just ":q" -> return ()
           Just line ->
             let parseRes = parse expr "⟨repl⟩" (T.pack line)
             in case parseRes of
                  Left err -> outputStrLn (errorBundlePretty err) *> repl
                  Right (ESyn e) -> outputStrLn (show (pretty e)) *> repl
                  Right (EChk e) -> outputStrLn (show (pretty e)) *> repl
