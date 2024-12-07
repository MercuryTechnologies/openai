# `openai-servant`

This provides a binding to OpenAI's API using `servant`

Example usage:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Data.Foldable (traverse_)
import OpenAI.Servant.V1
import OpenAI.Servant.V1.Chat.Completions

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

main :: IO ()
main = do
    key <- Environment.getEnv "OPENAI_KEY"

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    line <- Text.IO.getLine

    ChatCompletion{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = line, name = Nothing } ]
        , model = "gpt-4o-mini"
        }

    let display Choice{ message } = Text.IO.putStrLn (messageToContent message)

    traverse_ display choices
```
