{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Lang
import Parser hiding (synth)
import Pretty
import TC

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import System.IO
import System.Console.Haskeline
import System.Environment
import System.Exit
import Text.Megaparsec

main :: IO ()
main =
  getArgs >>=
  \case
    [] -> runInputT defaultSettings repl
    [f] ->
      do putStrLn $ "Processing " ++ f
         processFile f
    other -> printUsage
  where
    repl =
      do l <- getInputLine "> "
         case l of
           Nothing -> return ()
           Just ":q" -> return ()
           Just (T.pack -> line) ->
             let parseRes = parse expr "⟨repl⟩" line
             in case parseRes of
                  Left err -> outputStrLn (errorBundlePretty err) *> repl
                  Right (EChk e) ->
                    do outputStrLn "Got a checkable expression with no type:"
                       outputStrLn (T.unpack (highlightSpan line (view location e)))
                       repl
                  Right (ESyn e) ->
                    do outputStrLn "Parser output for synth:"
                       outputStrLn (show (pretty e))
                       outputStrLn "Type checking result:"
                       t <- liftIO $ tc (synth e) (emptyContext (view location e))
                       case t of
                         Left err ->
                           do outputStrLn (show (pretty (view value err)))
                              outputStrLn (T.unpack (highlightSpan line (view location err)))
                         Right t ->
                           outputStrLn (show (pretty t))
                       repl

processFile :: FilePath -> IO ()
processFile f =
  do contents <- T.readFile f
     let parseRes = parse prog f contents
     case parseRes of
       Left err ->
          putStrLn (errorBundlePretty err)
       Right program ->
         case (view declarations program) of
           (_:_) ->
             do putStrLn "Declarations don't work yet!"
                exitFailure
           [] ->
             do let e = view body program
                putStrLn "Parser put for body:"
                putStrLn (show (pretty e))
                putStrLn "Type checking result:"
                t <- liftIO $ tc (synth e) (emptyContext (view location e))
                case t of
                  Left err ->
                    do putStrLn (show (pretty (view value err)))
                       putStrLn (T.unpack (highlightSpan contents (view location err)))
                  Right t ->
                    putStrLn (show (pretty t))




highlightSpan :: Text -> Span -> Text
highlightSpan input (Span (Pos l1 c1) (Pos l2 c2)) =
  if l1 == l2
    then
      case findLine l1 input of
        Nothing -> T.empty
        Just line ->
          "\n  " <> line <>
          "\n  " <> spaces c1 <> caret <> dashes (c2 - c1 - 1) <> caret <> nl
    else
      case pair <$> findLine l1 input <*> findLine l2 input of
        Nothing -> T.empty
        Just (line1, line2) ->
          "\n  " <> spaces (T.length label1 + c1) <> vee <> dashes (max (T.length line1) (T.length line2) - 2) <>
          "\n  " <> label1 <> line1 <>
          (if l2 - l1 == 1 then "" else "\n" <> T.replicate (T.length label1) " " <>  "  ...") <>
          "\n  " <> label2 <> line2 <>
          "\n  " <> spaces (T.length label2 + 1) <> dashes (c2 - 1) <> caret <> nl
  where
    (label1, label2) =
      let l1str = show l1
          l2str = show l2
      in
        ((T.replicate (length l1str - length l2str) " " <> T.pack l1str <> ": "),
         (T.replicate (length l2str - length l1str) " " <> T.pack l2str <> ": "))
    pair x y = (x, y)
    caret = "^"
    vee = "v"
    nl = "\n"
    spaces n = T.replicate (n - 1) " "
    dashes n = T.replicate (n - 1) "-"

findLine :: Int -> Text -> Maybe Text
findLine n input = find' n (T.lines input)
  where
    find' n (l:ls)
      | n <= 1 = Just l
      | otherwise = find' (n - 1) ls
    find' _ [] = Nothing


printUsage :: IO ()
printUsage =
  traverse_ putStrLn
    [ "Usage:"
    , "No arguments means REPL"
    , "One argument means process a file"
    , "More arguments means see this message"
    ]
