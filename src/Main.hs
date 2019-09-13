{-# LANGUAGE DeriveFunctor #-}
module Main where

import Lang
import Parser hiding (synth)
import Pretty
import TC

import Control.Lens
import Control.Monad.IO.Class
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
                  Right (EChk e) ->
                    do outputStrLn "Got a checkable expression with no type:"
                       outputStrLn (show (pretty e))
                       repl
                  Right (ESyn e) ->
                    do outputStrLn "Parser output for synth:"
                       outputStrLn (show (pretty e))
                       outputStrLn "Type checking result:"
                       t <- liftIO $ tc (synth e) (emptyContext (view location e))
                       case t of
                         Left err ->
                           outputStrLn (show err)
                         Right t ->
                           outputStrLn (show (pretty t))
                       repl
