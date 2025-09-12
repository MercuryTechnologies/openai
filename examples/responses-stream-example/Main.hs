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
            , Responses.input = Just [Responses.Item_InputMessage{ Responses.role = Responses.User, Responses.content = [Responses.Input_Text "Write a short haiku about the sea."], Responses.status = Nothing }]
            }

    createResponseStreamTyped reqHaiku onEvent
    
    putStrLn "--------------------------------"

    -- 2) Web search example
    let reqSearch = Responses._CreateResponse
            { Responses.model = "gpt-5-mini"
            , Responses.input = Just [Responses.Item_InputMessage{ Responses.role = Responses.User, Responses.content = [Responses.Input_Text "Use web_search to find current news about France and display a concise summary. Include citations."], Responses.status = Nothing }]
            , Responses.tools = Just [ Tool.Tool_Web_Search ]
            }

    createResponseStreamTyped reqSearch onEvent

    putStrLn "--------------------------------"

    -- 3) Code interpreter example (per docs)
    let reqCode = Responses._CreateResponse
            { Responses.model = "gpt-5-mini"
            , Responses.instructions = Just "You are a personal math tutor. When asked a math question, write and run code using the python tool to answer the question."
            , Responses.input = Just [Responses.Item_InputMessage{ Responses.role = Responses.User, Responses.content = [Responses.Input_Text "I need to solve the equation 3x + 11 = 14. Can you help me?"], Responses.status = Nothing }]
            , Responses.tools = Just [ Tool.codeInterpreterAuto ]
            }

    createResponseStreamTyped reqCode onEvent
