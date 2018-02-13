-- #+TITLE: hs-outorg
-- #+AUTHOR: Jeremy Hughes
-- #+EMAIL: jedahu@gmail.com
-- #+DATE: [2017-07-26 Wed]

-- A command for outcommenting [[https://github.com/alphapapa/outorg][Outorg]]
-- formatted files.

-- Usage:
-- #+BEGIN_SRC sh :example t
-- outorg language comment-syntax [markup] < input > output.org
-- #+END_SRC

-- E.g. for this file:
-- #+BEGIN_SRC sh :example t
-- outorg haskell -- org < Main.hs > README.org
-- #+END_SRC

-- ** Module setup

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((+), error)
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
import Data.Maybe (Maybe(..), fromMaybe, listToMaybe)
import Data.Ord ((<), (>))
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

data Markup = Org | Md deriving Eq

doc2markup :: Text -> Markup
doc2markup "org"      = Org
doc2markup "md"       = Md
doc2markup "markdown" = Md
doc2markup x          = error ("Unrecognised markup name " <> T.unpack x)

codeStart :: Markup -> Text -> Text
codeStart Org lang = "#+BEGIN_SRC " <> lang
codeStart Md  lang = "``` " <> lang

codeStop :: Markup -> Text
codeStop Org = "#+END_SRC"
codeStop Md  = "```"

main :: IO ()
main = do
  args <- getArgs
  if intersect args ["-h", "--help"] /= [] || length args < 2 || length args > 3
    then printUsage >> exitFailure
    else let lang : comment : rest = T.pack <$> args
             doc                   = fromMaybe "org" (listToMaybe rest)
             markup                = doc2markup doc
         in interact (outcommentOutorg lang comment markup)

printUsage :: IO ()
printUsage = do
  putStrLn "usage:"
  putStrLn "    outorg language comment-syntax [org|md] < input > output.org"
  putStrLn "example:"
  putStrLn "    outorg javascript // org < index.js > README.org"

outcommentOutorg :: Text -> Text -> Markup -> Text -> Text
outcommentOutorg lang comment markup code =
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
      ["", codeStart markup lang]
      <>
      (unscoped <$> (stripBlanks xs))
      <>
      [codeStop markup]

    unscoped (_, s) = s

    is x (y, _) = x == y

    stripBlanks = dropWhileEnd (is Blank) . dropWhile (is Blank)

    group xs@((Code, _) : _)    = Just (break (is Comment) xs)
    group xs@((Comment, _) : _) = Just (break (is Code) xs)
    group xs@((Blank, _) : _)   = Just ([], tail xs)
    group []                    = Nothing
