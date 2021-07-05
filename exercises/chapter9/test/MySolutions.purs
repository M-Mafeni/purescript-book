module Test.MySolutions where

import Prelude

import Control.Parallel (parOneOf, parTraverse)
import Data.Array (concat, cons, fold) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, concat, dirname, relative)
import Test.HTTP (getUrl)

-- Note to reader: Add your solutions to this file
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles input1 input2 output = do
    input1Txt <- readTextFile UTF8 input1
    input2Txt <- readTextFile UTF8 input2
    writeTextFile UTF8 output (input1Txt <> input2Txt)
    pure unit

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany xs output = do
    arrContents <- traverse (readTextFile UTF8) xs
    writeTextFile UTF8 output $ A.fold arrContents

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters filepath = do
    result <- attempt $ readTextFile UTF8 filepath
    case result of
        Left e -> pure (Left e)
        Right content -> pure $ Right (length content)
  
writeGet :: String -> FilePath -> Aff Unit
writeGet url outputPath = do
  body <- getUrl url
  writeTextFile UTF8 outputPath body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel xs output = do
    arrContents <- parTraverse (readTextFile UTF8) xs
    writeTextFile UTF8 output $ A.fold arrContents

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout timeout url = parOneOf [delay (Milliseconds timeout) $> Nothing,Just <$> getUrl url]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles filePath = do
    content <- readTextFile UTF8 filePath
    if length content == 0 
        then pure [filePath]
        else do
            let childPaths = split (Pattern "\n") content :: Array FilePath
            -- recurseFiles :: FP -> Aff (Array FP)
            -- (map (\childPath -> concat [filePath, childPath]) childPaths) :: Array FP
            -- Aff (Array (Array FP))
            vals <- parTraverse recurseFiles (map (\childPath -> concat [dirname filePath, childPath]) childPaths)
            pure $ A.cons filePath (A.concat vals)