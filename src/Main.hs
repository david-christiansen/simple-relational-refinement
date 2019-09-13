{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Lang
import Parser hiding (synth)
import Pretty
import TC

import Control.Lens
import Control.Monad.IO.Class
import Data.Text (Text)
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
           Just (T.pack -> line) ->
             let parseRes = parse expr "⟨repl⟩" line
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
                           do outputStrLn (show err)
                              outputStrLn (T.unpack (highlightSpan line (view location err)))
                         Right t ->
                           outputStrLn (show (pretty t))
                       repl



highlightSpan :: Text -> Span -> Text
highlightSpan input (Span (Pos l1 c1) (Pos l2 c2)) =
  if l1 == l2
    then
      case findLine l1 input of
        Nothing -> T.empty
        Just line ->
          T.pack "\n  " <> line <>
          T.pack "\n  " <> spaces c1 <> caret <> dashes (c2 - c1 - 1) <> caret <> nl
    else
      case pair <$> findLine l1 input <*> findLine l2 input of
        Nothing -> T.empty
        Just (line1, line2) ->
          T.pack "\n  " <> spaces (T.length label1 + c1) <> vee <> dashes (max (T.length line1) (T.length line2) - 2) <>
          T.pack "\n  " <> label1 <> line1 <>
          T.pack (if l2 - l1 == 1 then "" else "\n" ++ replicate (T.length label1) ' ' ++  "  ...") <>
          T.pack "\n  " <> label2 <> line2 <>
          T.pack "\n  " <> spaces (T.length label2 + 1) <> dashes (c2 - 1) <> caret <> nl
  where
    (label1, label2) =
      let l1str = show l1
          l2str = show l2
      in
        (T.pack (replicate (length l1str - length l2str) ' ' ++ l1str ++ ": "),
         T.pack (replicate (length l2str - length l1str) ' ' ++ l2str ++ ": "))
    pair x y = (x, y)
    caret = T.pack "^"
    vee = T.pack "v"
    nl = T.pack "\n"
    spaces n = T.replicate (n - 1) (T.pack " ")
    dashes n = T.replicate (n - 1) (T.pack "-")

findLine :: Int -> Text -> Maybe Text
findLine n input = find' n (T.lines input)
  where
    find' n (l:ls)
      | n <= 1 = Just l
      | otherwise = find' (n - 1) ls
    find' _ [] = Nothing
