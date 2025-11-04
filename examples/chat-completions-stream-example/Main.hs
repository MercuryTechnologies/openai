{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import System.Environment (getEnv)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified OpenAI.V1 as V1
import qualified OpenAI.V1.Chat.Completions as Chat

main :: IO ()
main = do
    key <- T.pack <$> getEnv "OPENAI_KEY"
    env <- V1.getClientEnv "https://api.openai.com"

    let V1.Methods{ createChatCompletionStreamTyped } = V1.makeMethods env key Nothing Nothing

    let onEvent (Left err) = hPutStrLn stderr ("stream error: " <> T.unpack err)
        onEvent (Right chunk) = case chunk of
            -- Print content deltas as they arrive
            Chat.ChatCompletionChunk{ Chat.choices = cs } ->
                mapM_ printChoice cs
              where
                printChoice Chat.ChunkChoice{ Chat.delta = d } = case Chat.delta_content d of
                    Just content -> TIO.putStr content >> hFlush stdout
                    Nothing -> pure ()

    -- 1) Simple haiku example
    putStrLn "Example 1: Simple haiku"
    putStrLn "========================"
    let reqHaiku = Chat._CreateChatCompletion
            { Chat.messages =
                [ Chat.User
                    { Chat.content = [ Chat.Text{ Chat.text = "Write a short haiku about the sea." } ]
                    , Chat.name = Nothing
                    }
                ]
            , Chat.model = "gpt-5-mini"
            }

    createChatCompletionStreamTyped reqHaiku onEvent
    putStrLn "\n"

    putStrLn "--------------------------------"

    -- 2) Conversation example
    putStrLn "Example 2: Multi-turn conversation"
    putStrLn "===================================="
    let reqConversation = Chat._CreateChatCompletion
            { Chat.messages =
                [ Chat.System
                    { Chat.content = [ Chat.Text{ Chat.text = "You are a helpful assistant that explains concepts simply." } ]
                    , Chat.name = Nothing
                    }
                , Chat.User
                    { Chat.content = [ Chat.Text{ Chat.text = "What is quantum computing in simple terms?" } ]
                    , Chat.name = Nothing
                    }
                ]
            , Chat.model = "gpt-5-mini"
            , Chat.temperature = Just 0.7
            }

    createChatCompletionStreamTyped reqConversation onEvent
    putStrLn "\n"

    putStrLn "--------------------------------"

    -- 3) Code generation example
    putStrLn "Example 3: Code generation"
    putStrLn "=========================="
    let reqCode = Chat._CreateChatCompletion
            { Chat.messages =
                [ Chat.User
                    { Chat.content = [ Chat.Text{ Chat.text = "Write a Python function that calculates fibonacci numbers recursively." } ]
                    , Chat.name = Nothing
                    }
                ]
            , Chat.model = "gpt-5-mini"
            , Chat.temperature = Just 0.3
            }

    createChatCompletionStreamTyped reqCode onEvent
    putStrLn "\n"

