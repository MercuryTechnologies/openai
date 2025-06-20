{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM, when)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import OpenAI.V1.Tool
import OpenAI.V1.ToolCall
import qualified System.Environment as Environment

-- | Mock weather data for different cities
mockWeatherData :: Text -> Text
mockWeatherData city = case Text.toLower city of
  "london" -> "London: 18°C, partly cloudy, 60% humidity, light rain expected"
  "paris" -> "Paris: 22°C, sunny, 45% humidity, clear skies"
  "tokyo" -> "Tokyo: 28°C, humid, 80% humidity, thunderstorms possible"
  "new york" -> "New York: 25°C, partly sunny, 55% humidity, pleasant weather"
  "san francisco" -> "San Francisco: 16°C, foggy, 70% humidity, typical coastal weather"
  _ -> city <> ": Weather data not available for this location. Try London, Paris, Tokyo, New York, or San Francisco."

-- | Process a weather tool call and return the result
processWeatherToolCall :: ToolCall -> IO (Message (Vector Content))
processWeatherToolCall (ToolCall_Function toolCallId function) = do
  let functionName = OpenAI.V1.ToolCall.name function
      arguments = OpenAI.V1.ToolCall.arguments function

  putStrLn $ "🔧 Processing tool call: " <> Text.unpack functionName
  putStrLn $ "   Arguments: " <> Text.unpack arguments

  -- Parse arguments to extract city name
  let weatherResult = case Aeson.decode (ByteString.Lazy.fromStrict $ Text.Encoding.encodeUtf8 arguments) of
        Just (Object obj) -> case KeyMap.lookup "city" obj of
          Just (String cityName) -> mockWeatherData cityName
          _ -> "Error: Could not parse city name from arguments"
        _ -> "Error: Invalid arguments format"

  putStrLn $ "   Result: " <> Text.unpack weatherResult
  putStrLn ""

  -- Return a Tool message with the result
  return $
    Tool
      { content = [Text weatherResult],
        tool_call_id = toolCallId
      }

-- | Define the weather tool
weatherTool :: Tool
weatherTool =
  Tool_Function $
    OpenAI.V1.Tool.Function
      { description = Just "Get current weather information for a city",
        name = "get_weather",
        parameters =
          Just $
            Aeson.object
              [ "type" .= ("object" :: Text),
                "properties"
                  .= Aeson.object
                    [ "city"
                        .= Aeson.object
                          [ "type" .= ("string" :: Text),
                            "description" .= ("The city name to get weather for" :: Text)
                          ]
                    ],
                "required" .= (["city"] :: [Text]),
                "additionalProperties" .= False
              ],
        strict = Just True
      }

-- | Convert a Message Text to Message (Vector Content)
convertMessage :: Message Text -> Message (Vector Content)
convertMessage (System {content, name}) = System {content = [Text content], name = name}
convertMessage (User {content, name}) = User {content = [Text content], name = name}
convertMessage (Assistant {assistant_content, refusal, name, assistant_audio, tool_calls}) =
  Assistant
    { assistant_content = fmap (\c -> [Text c]) assistant_content,
      refusal = refusal,
      name = name,
      assistant_audio = assistant_audio,
      tool_calls = tool_calls
    }
convertMessage (Tool {content, tool_call_id}) = Tool {content = [Text content], tool_call_id = tool_call_id}

-- | Process all tool calls in a message and return tool response messages
processToolCalls :: Vector ToolCall -> IO (Vector (Message (Vector Content)))
processToolCalls toolCalls = do
  when (not $ Vector.null toolCalls) $ do
    putStrLn $ "🤖 Processing " <> show (Vector.length toolCalls) <> " tool call(s)..."
  Vector.mapM processWeatherToolCall toolCalls

-- | Main chat loop
chatLoop :: (CreateChatCompletion -> IO ChatCompletionObject) -> Vector (Message (Vector Content)) -> IO ()
chatLoop createChatCompletion messages = do
  Text.IO.putStr "You: "
  userInput <- Text.IO.getLine

  -- Exit condition
  when (Text.toLower userInput == "quit" || Text.toLower userInput == "exit") $ do
    putStrLn "Goodbye! 👋"
    return ()

  -- Add user message to conversation
  let userMessage =
        User
          { content = [Text userInput],
            name = Nothing
          }
      updatedMessages = messages <> [userMessage]

  -- Make API call with tool support
  response <-
    createChatCompletion
      _CreateChatCompletion
        { messages = updatedMessages,
          model = "gpt-4o-mini",
          tools = Just [weatherTool],
          tool_choice = Just ToolChoiceAuto
        }

  let ChatCompletionObject {choices} = response

  -- Process each choice (usually just one)
  newMessages <- foldM (processChoice createChatCompletion) updatedMessages choices

  -- Continue the conversation
  chatLoop createChatCompletion newMessages

-- | Process a single choice, handling potential tool calls
processChoice ::
  (CreateChatCompletion -> IO ChatCompletionObject) ->
  Vector (Message (Vector Content)) ->
  Choice ->
  IO (Vector (Message (Vector Content)))
processChoice createChatCompletion messages choice = do
  let Choice {message = assistantMessage} = choice

  case assistantMessage of
    Assistant {assistant_content, tool_calls = Just toolCalls} -> do
      -- Display assistant's message if any
      case assistant_content of
        Just content -> putStrLn $ "Assistant: " <> Text.unpack content
        Nothing -> return ()

      -- Add assistant message to history
      let messagesWithAssistant = messages <> [convertMessage assistantMessage]

      -- Process tool calls
      toolResults <- processToolCalls toolCalls
      let messagesWithTools = messagesWithAssistant <> toolResults

      -- Make another API call to get the final response after tool calls
      finalResponse <-
        createChatCompletion
          _CreateChatCompletion
            { messages = messagesWithTools,
              model = "gpt-4o-mini",
              tools = Just [weatherTool],
              tool_choice = Just ToolChoiceAuto
            }

      let ChatCompletionObject {choices = finalChoices} = finalResponse

      -- Process the final response (this should not have tool calls)
      foldM processFinalChoice messagesWithTools finalChoices
    Assistant {assistant_content = Just content} -> do
      -- No tool calls, just display the response
      putStrLn $ "Assistant: " <> Text.unpack content
      return $ messages <> [convertMessage assistantMessage]
    _ -> do
      putStrLn "Assistant: (No response content)"
      return messages

-- | Process final choice after tool calls (should just be text response)
processFinalChoice :: Vector (Message (Vector Content)) -> Choice -> IO (Vector (Message (Vector Content)))
processFinalChoice messages choice = do
  let Choice {message = finalMessage} = choice
  case finalMessage of
    Assistant {assistant_content = Just content} -> do
      putStrLn $ "Assistant: " <> Text.unpack content
      return $ messages <> [convertMessage finalMessage]
    _ -> do
      putStrLn "Assistant: (Unexpected response format)"
      return messages

main :: IO ()
main = do
  putStrLn "🌤️  Weather Chatbot with Tool Calling"
  putStrLn "======================================"
  putStrLn "Ask me about the weather in different cities!"
  putStrLn "Try: 'What's the weather like in London?'"
  putStrLn "Available cities: London, Paris, Tokyo, New York, San Francisco"
  putStrLn "Type 'quit' or 'exit' to end the conversation."
  putStrLn ""

  -- Get OpenAI API key
  key <- Environment.getEnv "OPENAI_KEY"

  -- Set up client
  clientEnv <- getClientEnv "https://api.openai.com"
  let Methods {createChatCompletion} = makeMethods clientEnv (Text.pack key)

  -- Initial system message
  let systemMessage =
        System
          { content = [Text "You are a helpful weather assistant. Use the get_weather tool to provide current weather information when users ask about weather in specific cities. Be conversational and helpful."],
            name = Nothing
          }

  -- Start the chat loop
  chatLoop createChatCompletion [systemMessage]