{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified OpenAI.V1 as V1
import qualified OpenAI.V1.Responses as Responses
import qualified OpenAI.V1.Tool as Tool
import System.Environment (getEnv)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

main :: IO ()
main = do
    key <- T.pack <$> getEnv "OPENAI_KEY"
    env <- V1.getClientEnv "https://api.openai.com"

    let V1.Methods{ createResponseStreamTyped } = V1.makeMethods env key Nothing Nothing

    let onEvent (Left err) = hPutStrLn stderr ("stream error: " <> T.unpack err)
        onEvent (Right ev) = case ev of
            -- Only print model text deltas and newline on part done
            Responses.ResponseTextDeltaEvent{ Responses.delta } ->
                TIO.putStr delta >> hFlush stdout
            Responses.ResponseTextDoneEvent{} -> putStrLn ""
            -- Ignore all other events for a clean output
            _ -> pure ()

    -- 1) Cute haiku test (no tools)
    let reqHaiku = Responses._CreateResponse
            { Responses.model = "gpt-5-mini"
            , Responses.input = Just (Responses.Input_String "Write a short haiku about the sea.")
            }

    createResponseStreamTyped reqHaiku onEvent
    
    putStrLn ""

    -- 2) Web search example (showcases web_search_call)
    let reqSearch = Responses._CreateResponse
            { Responses.model = "gpt-5"
            , Responses.input = Just (Responses.Input_String "Use web_search to find current news about France and display a concise summary.")
            , Responses.tools = Just [ Tool.Tool_Web_Search ]
            }

    createResponseStreamTyped reqSearch onEvent
