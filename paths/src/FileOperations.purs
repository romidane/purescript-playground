module FileOperations where

import Data.Foldable
import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, (:), head)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Path (Path, filename, isDirectory, ls, size, root)
-- import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

onlyFiles' :: Path -> Array Path
onlyFiles' file = do
  child <- ls file

  if isDirectory child
    then onlyFiles' child
    else pure child

largestFile :: Path -> Maybe Path
largestFile = foldl largest Nothing <<< onlyFiles
  where
    largest :: Maybe Path -> Path -> Maybe Path
    largest Nothing path = Just path
    largest (Just currentFile) nextPath =
      if size currentFile > size nextPath
        then Just currentFile
        else Just nextPath

smallestFile :: Path -> Maybe Path
smallestFile = foldl smallest Nothing <<< onlyFiles
  where
    smallest :: Maybe Path -> Path -> Maybe Path
    smallest Nothing path = Just path
    smallest (Just currentFile) nextPath =
      if size currentFile < size nextPath
        then Just currentFile
        else Just nextPath

whereIs :: String -> Maybe Path
whereIs name = head $ do
  path <- allFiles root
  file <- ls path

  guard $ filename file == name

  pure path