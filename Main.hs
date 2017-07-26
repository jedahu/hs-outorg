-- #+TITLE: hs-outorg
-- #+AUTHOR: Jeremy Hughes
-- #+EMAIL: jedahu@gmail.com
-- #+DATE: [2017-07-26 Wed]

-- A command for outcommenting [[https://github.com/alphapapa/outorg][Outorg]]
-- formatted files.

-- Usage:
-- #+BEGIN_SRC sh :example t
-- outorg language comment-syntax < input > output.org
-- #+END_SRC

-- E.g. for this file:
-- #+BEGIN_SRC sh :example t
-- outorg haskell -- < Main.hs > README.org
-- #+END_SRC

-- ** Module setup

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((+))
import Data.Bool ((||))
import Data.Eq (Eq, (==), (/=))
import Data.Functor ((<$>))
import Data.List
  ( dropWhile
  , dropWhileEnd
  , break
  , unfoldr
  , tail
  , length
  , intersect
  )
import Data.Maybe (Maybe(..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO (interact, putStrLn)
import Data.Semigroup ((<>))
import Control.Category ((.))
import Control.Monad ((=<<), (>>))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IO(..))


-- ** Implementation

-- 1. Tag lines as comments, code, or blank.
-- 2. Group lines by code and comment.
-- 3. Wrap code in org src block.
-- 4. Join blocks with blank lines.

data Scope = Comment | Code | Blank deriving Eq

main :: IO ()
main = do
  args <- getArgs
  if intersect args ["-h", "--help"] /= [] || length args /= 2
    then printUsage >> exitFailure
    else let [lang, comment] = args
         in interact (outcommentOutorg (T.pack lang) (T.pack comment))

printUsage :: IO ()
printUsage = do
  putStrLn "usage:"
  putStrLn "    outorg language comment-syntax < input > output.org"
  putStrLn "example:"
  putStrLn "    outorg javascript // < index.js > README.org"

outcommentOutorg :: Text -> Text -> Text -> Text
outcommentOutorg lang comment code =
  T.unlines (dropWhile T.null (wrapBlocks =<< groupedLines))
  where
    groupedLines = unfoldr group (scoped <$> (T.lines code))

    commentSpace = comment <> " "

    scoped line = if isComment
                  then (Comment, T.drop commentLength line)
                  else if isBlank
                       then (Blank, "")
                       else (Code, line)
      where
        isBlank = T.null (T.strip line)
        isComment = T.isPrefixOf comment line
        hasSpace = T.isPrefixOf commentSpace line
        commentLength = T.length comment + if hasSpace then 1 else 0

    wrapBlocks []                    = []
    wrapBlocks xs@((Comment, _) : _) = "" : (unscoped <$> (stripBlanks xs))
    wrapBlocks xs@((Code, _) : _)    =
      ["", "#+BEGIN_SRC " <> lang]
      <>
      (unscoped <$> (stripBlanks xs))
      <>
      ["#+END_SRC"]

    unscoped (_, s) = s

    is x (y, _) = x == y

    stripBlanks = dropWhileEnd (is Blank) . dropWhile (is Blank)

    group xs@((Code, _) : _)    = Just (break (is Comment) xs)
    group xs@((Comment, _) : _) = Just (break (is Code) xs)
    group xs@((Blank, _) : _)   = Just ([], tail xs)
    group []                    = Nothing
