{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Foldable (toList)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified OpenAI.V1 as V1
import qualified OpenAI.V1.Responses as Responses
import System.Environment (getEnv)

main :: IO ()
main = do
    key <- Text.pack <$> getEnv "OPENAI_KEY"
    env <- V1.getClientEnv "https://api.openai.com"

    let V1.Methods{ createResponse } = V1.makeMethods env key Nothing Nothing

    let req = Responses._CreateResponse
            { Responses.model = "gpt-5"
            , Responses.input = Just (Responses.Input_String "Tell me a three sentence bedtime story about a unicorn.")
            }

    resp <- createResponse req

    let texts = collectText resp
    mapM_ TextIO.putStrLn texts

collectText :: Responses.ResponseObject -> [Text.Text]
collectText Responses.ResponseObject{ Responses.output } = do
    Responses.Item_OutputMessage Responses.OutputMessage{ Responses.content } <- toList output
    Responses.Output_Text{ Responses.text } <- toList content
    return text
