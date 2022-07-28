{-
   Copyright (c) 2022, Kevin Cheung
   All rights reserved.
   
   This source code is licensed under the BSD-style license found in the
   LICENSE file in the root directory of this source tree.
-}

module Main where

import System.Random
import Math.Combinat.Permutations (randomPermutation, permuteList)
import qualified Data.Text.IO as TI
import qualified Data.Text as T
import Control.Monad (mapM_)

import Params
import Question

lineBreak :: T.Text
lineBreak = T.pack "\n"

toText'Answer :: MultipleChoice -> (T.Text, Label)
toText'Answer (MultipleChoice question choices) = (questionText, answer)
  where
    cs = zip ['A'..'Z'] choices
    cts = map (\(label, (_, body)) -> T.pack ("    " ++ label:")") <> body) cs
    questionText = T.pack "1.  " <> question <> lineBreak <> lineBreak 
                  <> T.intercalate lineBreak cts <> lineBreak
    answer = fst $ head (filter (fst . snd) cs)

printAnswerKey :: [Label] -> IO ()
printAnswerKey ans = do
  mapM_ (\c -> putStrLn $ "1. "++ pure c) ans

printQuestions :: [T.Text] -> IO ()
printQuestions qs = do
  putStrLn "# Questions"
  putStrLn ""
  TI.putStrLn $ T.intercalate lineBreak qs

genTest :: String -> [MultipleChoice] -> IO ()
genTest title qs = do
  let (qts, ans) = unzip (map toText'Answer qs)
  putStrLn $ "% " ++ title 
  putStrLn ""
  printQuestions qts
  putStrLn "\\newpage"
  putStrLn ""
  putStrLn $ "# Answer key to " ++ title
  putStrLn ""
  printAnswerKey ans

work :: Params -> IO ()
work params = do
  let fname = filename params
  let count = questionCount params
  let testTitle = title params

  contents <- TI.readFile fname
  case parseAiken contents of
    Nothing -> print "Failed to parse file."
    Just qs -> let numQ = length qs in
               case count of
                 Nothing -> genTest testTitle qs
                 Just c -> do 
                   perm <- getStdRandom (randomPermutation numQ)
                   genTest testTitle (take c $ permuteList perm qs)

main :: IO ()
main = cmdLineParser >>= work
