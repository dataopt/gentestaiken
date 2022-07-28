{-
   Copyright (c) 2022, Kevin Cheung
   All rights reserved.
   
   This source code is licensed under the BSD-style license found in the
   LICENSE file in the root directory of this source tree.
-}

module Params (Params (..), cmdLineParser) where

import Options.Applicative

data Params = Params {
                filename :: FilePath
              , questionCount :: Maybe Int
              , title :: String
              }

optsParser :: Parser Params
optsParser =
  Params <$> strArgument (
               metavar "FILENAME"
               <> help "File name of question bank in Aiken format")
         <*> optional (
               option auto (long "random" <> short 'r' <> metavar "COUNT"
               <> help "Number of random-selected questions. Otherwise, all questions used unshuffled."))
         <*> strOption (
               long "title" <> short 't' <> metavar "TITLE"
               <> help "Test title." <> showDefault <> value "Test")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (optsParser <**> helper)
                (fullDesc <> progDesc "Generate test from Aiken question bank.")

