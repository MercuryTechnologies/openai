{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import OpenAI.V1.Responses (Tool(..))
import OpenAI.V1.Tool (CodeInterpreterContainer(..))
import System.Environment (getEnv)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified OpenAI.V1 as V1
import qualified OpenAI.V1.Responses as Responses

main :: IO ()
main = do
    key <- T.pack <$> getEnv "OPENAI_KEY"
    env <- V1.getClientEnv "https://api.openai.com"

    let V1.Methods{ createResponseStreamTyped } = V1.makeMethods env key Nothing Nothing

    let onEvent (Left err) = hPutStrLn stderr ("stream error: " <> T.unpack err)
        onEvent (Right ev) = case ev of
            -- Only print model text deltas and newline on part done
            Responses.ResponseTextDeltaEvent{ Responses.delta = d } ->
                TIO.putStr d >> hFlush stdout
            Responses.ResponseTextDoneEvent{} -> putStrLn ""
            -- Ignore all other events for a clean output
            _ -> pure ()

    -- 1) Cute haiku test (no tools)
    let reqHaiku = Responses._CreateResponse
            { Responses.model = "gpt-5-mini"
            , Responses.input = Just (Responses.Input
                [ Responses.Item_Input_Message
                    { Responses.role = Responses.User
                    , Responses.content = [ Responses.Input_Text{ Responses.text = "Write a short haiku about the sea." } ]
                    , Responses.status = Nothing
                    }
                ])
            }

    createResponseStreamTyped reqHaiku onEvent

    putStrLn "--------------------------------"

    -- 2) Web search example
    let reqSearch = Responses._CreateResponse
            { Responses.model = "gpt-5-mini"
            , Responses.input = Just (Responses.Input
                [ Responses.Item_Input_Message
                    { Responses.role = Responses.User
                    , Responses.content = [ Responses.Input_Text{ Responses.text = "Use web_search to find current news about France and display a concise summary. Do not include citations, references, or URLs in the output; provide only the summary text." } ]
                    , Responses.status = Nothing
                    }
                ])
            , Responses.tools = Just [ Tool_Web_Search ]
            }

    createResponseStreamTyped reqSearch onEvent

    putStrLn "--------------------------------"

    -- 3) Code interpreter example (per docs)
    let reqCode = Responses._CreateResponse
            { Responses.model = "gpt-5-mini"
            , Responses.instructions = Just "You are a personal math tutor. When asked a math question, write and run code using the python tool to answer the question."
            , Responses.input = Just (Responses.Input
                [ Responses.Item_Input_Message
                    { Responses.role = Responses.User
                    , Responses.content = [ Responses.Input_Text{ Responses.text = "I need to solve the equation 3x + 11 = 14. Can you help me?" } ]
                    , Responses.status = Nothing
                    }
                ])
            , Responses.tools = Just
                [ Responses.Tool_Code_Interpreter
                    { container = Just CodeInterpreterContainer_Auto
                        { file_ids = Nothing
                        }
                    }
                ]
            }

    createResponseStreamTyped reqCode onEvent
