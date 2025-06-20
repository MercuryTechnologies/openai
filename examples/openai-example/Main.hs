{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Data.Foldable (traverse_)
import OpenAI.V1
import OpenAI.V1.Chat.Completions

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

main :: IO ()
main = do
    key <- Environment.getEnv "OPENAI_KEY"

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    text <- Text.IO.getLine

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text } ], name = Nothing } ]
        , model = "gpt-4o-mini"
        }

    let display Choice{ message } = Text.IO.putStrLn (messageToContent message)

    traverse_ display choices
