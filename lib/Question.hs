{-
   Copyright (c) 2022, Kevin Cheung
   All rights reserved.
   
   This source code is licensed under the BSD-style license found in the
   LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

module Question where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT

type QuestionStatement = T.Text
type Label = Char
type Choice = (Bool, T.Text)

data MultipleChoice = MultipleChoice QuestionStatement [Choice] deriving Show

choiceParser :: AT.Parser (Label, T.Text)
choiceParser = do
  label <- AT.letter
  _ <- AT.char ')' <> AT.char '.'
  body <- AT.takeTill AT.isEndOfLine
  AT.endOfLine
  return (label, body)

answerParser :: [Label] -> AT.Parser Label
answerParser labels = do
  _ <- AT.string "ANSWER: "
  choice <- validLabel
  AT.endOfLine
  return choice
  where validLabel = AT.satisfy (`elem` labels)

questionTextParser :: AT.Parser T.Text
questionTextParser = do
  question <- AT.takeTill AT.isEndOfLine
  AT.endOfLine
  return question

aikenQuestionParser :: AT.Parser MultipleChoice
aikenQuestionParser = do
  question <- questionTextParser
  choices <- AT.many' choiceParser 
  correct <- answerParser $ map fst choices
  _ <- AT.skipSpace
  let c = map (\(label, body) -> (label==correct, body)) choices
  return $ MultipleChoice question c

aikenParser :: AT.Parser [MultipleChoice]
aikenParser = do 
  questions <- AT.many' aikenQuestionParser
  _ <- AT.endOfInput
  return questions

parseAiken :: T.Text -> Maybe [MultipleChoice]
parseAiken t = case AT.parseOnly aikenParser t of
                 Left _ -> Nothing
                 Right ms -> Just ms
