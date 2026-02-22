{-# LANGUAGE BlockArguments, DuplicateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings, RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables                                   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Exception (SomeException, catch)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import OpenAI.V1 (Methods(..))
import OpenAI.V1.Audio.Speech (CreateSpeech(..), Voice(..), _CreateSpeech)
import OpenAI.V1.Audio.Transcriptions (CreateTranscription(..))
import OpenAI.V1.Audio.Translations (CreateTranslation(..))
import OpenAI.V1.AutoOr (AutoOr(..))
import OpenAI.V1.Batches (BatchObject(..), CreateBatch(..))
import OpenAI.V1.Chat.Completions (CreateChatCompletion(..), Modality(..))
import OpenAI.V1.Embeddings (CreateEmbeddings(..), EncodingFormat(..))
import OpenAI.V1.Files (FileObject(..), Order(..), UploadFile(..))
import OpenAI.V1.Images.Edits (CreateImageEdit(..))
import OpenAI.V1.Images.Generations (CreateImage(..), Quality(..), Style(..))
import OpenAI.V1.Images.Variations (CreateImageVariation(..))
import OpenAI.V1.Message (Message(..))
import OpenAI.V1.Moderations (CreateModeration(..))
import OpenAI.V1.Threads.Messages (MessageObject(..), ModifyMessage(..))
import OpenAI.V1.Tool (CodeInterpreterContainer(..), Tool(..), ToolChoice(..))
import OpenAI.V1.ToolCall (ToolCall(..))
import Prelude hiding (id)

import OpenAI.V1.Assistants
    (AssistantObject(..), CreateAssistant(..), ModifyAssistant(..))
import OpenAI.V1.FineTuning.Jobs
    (CreateFineTuningJob(..), Hyperparameters(..), JobObject(..))
import OpenAI.V1.Threads
    (ModifyThread(..), Thread(..), ThreadID(..), ThreadObject(..))
import OpenAI.V1.Threads.Runs
    (CreateRun(..), ModifyRun(..), RunID(..), RunObject(..), Status(..))
import OpenAI.V1.Uploads
    ( AddUploadPart(..)
    , CompleteUpload(..)
    , CreateUpload(..)
    , PartObject(..)
    , UploadObject(..)
    )
import OpenAI.V1.VectorStores
    (CreateVectorStore(..), ModifyVectorStore(..), VectorStoreObject(..))
import OpenAI.V1.VectorStores.FileBatches
    (CreateVectorStoreFileBatch(..), VectorStoreFilesBatchObject(..))
import OpenAI.V1.VectorStores.Files
    (CreateVectorStoreFile(..), VectorStoreFileObject(..))

import qualified Control.Concurrent as Concurrent
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified OpenAI.V1 as V1
import qualified OpenAI.V1.Chat.Completions as Completions
import qualified OpenAI.V1.Chat.Completions.Stream as ChatStream
import qualified OpenAI.V1.Files as Files
import qualified OpenAI.V1.FineTuning.Jobs as Jobs
import qualified OpenAI.V1.Images.ResponseFormat as ResponseFormat
import qualified OpenAI.V1.Responses as Responses
import qualified OpenAI.V1.Tool as Tool
import qualified OpenAI.V1.ToolCall as ToolCall
import qualified Servant.Client as Client
import qualified System.Environment as Environment
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
  let managerSettings =
        TLS.tlsManagerSettings
          { HTTP.Client.managerResponseTimeout =
              HTTP.Client.responseTimeoutNone
          }

  manager <- TLS.newTlsManagerWith managerSettings

  baseUrl <- Client.parseBaseUrl "https://api.openai.com"

  let clientEnv = Client.mkClientEnv manager baseUrl

  key <- Environment.getEnv "OPENAI_KEY"

  let user = "openai Haskell package"
  let chatModel = "gpt-4o-mini"
  let reasoningModel = "gpt-5.2-2025-12-11"
  let ttsModel = "tts-1"
  let Methods {..} = V1.makeMethods clientEnv (Text.pack key) Nothing Nothing

  -- Test each format to make sure we're handling each possible content type
  -- correctly
  let speechTest format =
        HUnit.testCase ("Create speech - " <> show format) do
          _ <-
            createSpeech
              _CreateSpeech
                { model = ttsModel,
                  input = "Hello, world!",
                  voice = Nova,
                  response_format = Just format,
                  speed = Just 1.0
                }

          return ()

  let speechTestDefaults =
        HUnit.testCase "Create speech - defaults" do
          _ <-
            createSpeech
              _CreateSpeech
                { model = ttsModel,
                  input = "Hello, world!",
                  voice = Alloy
                }

          return ()

  let speechTests =
        speechTestDefaults : do
          format <- [minBound .. maxBound]
          return (speechTest format)

  let transcriptionTest =
        HUnit.testCase "Create transcription" do
          _ <-
            createTranscription
              CreateTranscription
                { file = "tasty/data/v1/audio/preamble.wav",
                  model = "whisper-1",
                  language = Just "en",
                  prompt = Nothing,
                  temperature = Just 0
                }

          return ()

  let translationTest =
        HUnit.testCase "Create translation" do
          _ <-
            createTranslation
              CreateTranslation
                { file = "tasty/data/v1/audio/preamble.wav",
                  model = "whisper-1",
                  prompt = Nothing,
                  temperature = Just 0
                }

          return ()

  let completionsMinimalTest =
        HUnit.testCase "Create chat completion - minimal" do
          _ <-
            createChatCompletion
              CreateChatCompletion
                { messages =
                    [ Completions.User
                        { content = ["Hello, world!"],
                          name = Nothing
                        }
                    ],
                  model = chatModel,
                  store = Nothing,
                  metadata = Nothing,
                  frequency_penalty = Nothing,
                  logit_bias = Nothing,
                  logprobs = Nothing,
                  top_logprobs = Nothing,
                  max_completion_tokens = Nothing,
                  n = Nothing,
                  modalities = Nothing,
                  prediction = Nothing,
                  audio = Nothing,
                  presence_penalty = Nothing,
                  reasoning_effort = Nothing,
                  response_format = Nothing,
                  seed = Nothing,
                  service_tier = Nothing,
                  stop = Nothing,
                  stream = Nothing,
                  stream_options = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  tools = Nothing,
                  tool_choice = Nothing,
                  parallel_tool_calls = Nothing,
                  user = Nothing,
                  web_search_options = Nothing
                }

          return ()

  let completionsMinimalReasoningTest =
        HUnit.testCase "Create chat completion reasoning model - minimal" do
          _ <-
            createChatCompletion
              CreateChatCompletion
                { messages =
                    [ Completions.User
                        { content = ["Hello, world!"],
                          name = Nothing
                        }
                    ],
                  model = reasoningModel,
                  store = Nothing,
                  metadata = Nothing,
                  frequency_penalty = Nothing,
                  logit_bias = Nothing,
                  logprobs = Nothing,
                  top_logprobs = Nothing,
                  max_completion_tokens = Nothing,
                  n = Nothing,
                  modalities = Nothing,
                  prediction = Nothing,
                  audio = Nothing,
                  presence_penalty = Nothing,
                  reasoning_effort = Just Completions.ReasoningEffort_Low,
                  response_format = Nothing,
                  seed = Nothing,
                  service_tier = Nothing,
                  stop = Nothing,
                  stream = Nothing,
                  stream_options = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  tools = Nothing,
                  tool_choice = Nothing,
                  parallel_tool_calls = Nothing,
                  user = Nothing,
                  web_search_options = Nothing
                }

          return ()

  let completionsMaximalTest =
        HUnit.testCase "Create chat completion - maximal" do
          _ <-
            createChatCompletion
              CreateChatCompletion
                { messages =
                    [ Completions.User
                        { content = ["Hello, world!"],
                          name = Just "gabby"
                        },
                      Completions.Assistant
                        { assistant_content = Nothing,
                          refusal = Nothing,
                          name = Just "Ada",
                          assistant_audio = Nothing,
                          tool_calls =
                            Just
                              [ ToolCall_Function
                                  { id = "call_bzE95mjMMFqeanfY2sL6Sdir",
                                    function =
                                      ToolCall.Function
                                        { name = "hello",
                                          arguments = "{}"
                                        }
                                  }
                              ]
                        },
                      Completions.Tool
                        { content = ["Hello, world!"],
                          tool_call_id = "call_bzE95mjMMFqeanfY2sL6Sdir"
                        }
                    ],
                  model = chatModel,
                  store = Just False,
                  metadata = Nothing,
                  frequency_penalty = Just 0,
                  logit_bias = Just mempty,
                  logprobs = Just True,
                  top_logprobs = Just 1,
                  max_completion_tokens = Just 1024,
                  n = Just 1,
                  modalities = Just [Modality_Text],
                  prediction = Nothing,
                  audio = Nothing,
                  presence_penalty = Just 0,
                  reasoning_effort = Nothing,
                  response_format = Just Completions.ResponseFormat_Text,
                  seed = Just 0,
                  service_tier = Just Auto,
                  stop = Just [">>>"],
                  stream = Nothing,
                  stream_options = Nothing,
                  temperature = Just 1,
                  top_p = Just 1,
                  tools =
                    Just
                      [ Tool_Function
                          { function =
                              Tool.Function
                                { description =
                                    Just "Use the hello command line tool",
                                  name = "hello",
                                  parameters = Nothing,
                                  strict = Just False
                                }
                          }
                      ],
                  tool_choice = Just ToolChoiceAuto,
                  parallel_tool_calls = Just True,
                  user = Just user,
                  web_search_options = Nothing
                }

          return ()

  let chatCompletionStreamingHaikuTest = do
        HUnit.testCase "Create chat completion - streaming haiku" do
              let req =
                    CreateChatCompletion
                        { messages =
                            [ Completions.User
                                { content = ["Hello, world!"],
                                  name = Nothing
                                }
                            ],
                          model = chatModel,
                          store = Nothing,
                          metadata = Nothing,
                          frequency_penalty = Nothing,
                          logit_bias = Nothing,
                          logprobs = Nothing,
                          top_logprobs = Nothing,
                          max_completion_tokens = Nothing,
                          n = Nothing,
                          modalities = Nothing,
                          prediction = Nothing,
                          audio = Nothing,
                          presence_penalty = Nothing,
                          reasoning_effort = Nothing,
                          response_format = Nothing,
                          seed = Nothing,
                          service_tier = Nothing,
                          stop = Nothing,
                          stream = Nothing,
                          stream_options = Nothing,
                          temperature = Nothing,
                          top_p = Nothing,
                          tools = Nothing,
                          tool_choice = Nothing,
                          parallel_tool_calls = Nothing,
                          user = Nothing,
                          web_search_options = Nothing
                        }

              acc <- IORef.newIORef (Text.empty)
              done <- Concurrent.newEmptyMVar

              let onEvent (Left _err) = Concurrent.putMVar done ()
                  onEvent (Right ev) = case ev of
                    ChatStream.ChatCompletionChunk{ ChatStream.choices = cs } ->
                        mapM_ accChoice cs
                      where
                        accChoice ChatStream.ChunkChoice{ ChatStream.delta = d } = case ChatStream.delta_content d of
                            Just content -> IORef.modifyIORef' acc (<> content)
                            Nothing -> Concurrent.putMVar done ()

              createChatCompletionStreamTyped req onEvent

              _ <- Concurrent.takeMVar done
              text <- IORef.readIORef acc
              HUnit.assertBool "Expected non-empty streamed text" (not (Text.null text))

              return ()
          

  let embeddingsTest = do
        HUnit.testCase "Create embedding" do
          _ <-
            createEmbeddings
              CreateEmbeddings
                { input = "Hello, world!",
                  model = "text-embedding-3-small",
                  encoding_format = Just Float,
                  dimensions = Just 1024,
                  user = Just user
                }

          return ()

  let fineTuningTest = do
        HUnit.testCase "Fine-tuning and File operations - maximal" do
          FileObject {id = trainingId} <-
            uploadFile
              UploadFile
                { file =
                    "tasty/data/v1/fine_tuning/jobs/training_data.jsonl",
                  purpose = Files.Fine_Tune
                }

          FileObject {id = validationId} <-
            uploadFile
              UploadFile
                { file =
                    "tasty/data/v1/fine_tuning/jobs/validation_data.jsonl",
                  purpose = Files.Fine_Tune
                }

          _ <- retrieveFile trainingId

          _ <- retrieveFileContent trainingId

          _ <- listFiles (Just Files.Fine_Tune) (Just 10000) (Just Asc) Nothing

          JobObject {id} <-
            createFineTuningJob
              CreateFineTuningJob
                { model = "gpt-4o-mini-2024-07-18",
                  training_file = trainingId,
                  hyperparameters =
                    Just
                      Hyperparameters
                        { batch_size = Just Jobs.Auto,
                          learning_rate_multiplier = Just Jobs.Auto,
                          n_epochs = Just Jobs.Auto
                        },
                  suffix = Just "haskell-openai",
                  validation_file = Just validationId,
                  integrations = Just [],
                  seed = Just 0
                }

          _ <- retrieveFineTuningJob id

          _ <- listFineTuningJobs Nothing (Just 20)

          _ <- listFineTuningCheckpoints id Nothing (Just 10)

          _ <- cancelFineTuning id

          _ <- listFineTuningEvents id Nothing (Just 20)

          _ <- deleteFile trainingId
          _ <- deleteFile validationId

          return ()

  let batchesTest = do
        HUnit.testCase "Batch operations" do
          FileObject {id = requestsId} <-
            uploadFile
              UploadFile
                { file = "tasty/data/v1/batches/requests.jsonl",
                  purpose = Files.Batch
                }

          BatchObject {id} <-
            createBatch
              CreateBatch
                { input_file_id = requestsId,
                  endpoint = "/v1/chat/completions",
                  completion_window = "24h",
                  metadata = Nothing
                }

          _ <- retrieveBatch id

          _ <- listBatch Nothing (Just 20)

          _ <- cancelBatch id

          return ()

  let uploadsTest = do
        HUnit.testCase "Upload operations" do
          UploadObject {id = cancelledId} <-
            createUpload
              CreateUpload
                { filename = "training_data.jsonl",
                  purpose = Files.Fine_Tune,
                  bytes = 4077,
                  mime_type = "text/jsonl"
                }

          _ <- cancelUpload cancelledId

          UploadObject {id} <-
            createUpload
              CreateUpload
                { filename = "training_data.jsonl",
                  purpose = Files.Fine_Tune,
                  bytes = 4077,
                  mime_type = "text/jsonl"
                }

          PartObject {id = partId0} <-
            addUploadPart
              id
              AddUploadPart
                { data_ = "tasty/data/v1/uploads/training_data0.jsonl"
                }

          PartObject {id = partId1} <-
            addUploadPart
              id
              AddUploadPart
                { data_ = "tasty/data/v1/uploads/training_data1.jsonl"
                }

          _ <-
            completeUpload
              id
              CompleteUpload
                { part_ids = [partId0, partId1],
                  md5 = Nothing
                }

          return ()

  let createImageMaximalTest = do
        HUnit.testCase "Create image - maximal" do
          _ <-
            createImage
              CreateImage
                { prompt = "A baby panda",
                  model = Just "dall-e-3",
                  n = Just 1,
                  quality = Just Standard,
                  response_format = Just ResponseFormat.URL,
                  size = Just "1024x1024",
                  style = Just Vivid,
                  user = Just user
                }

          return ()

  let createImageEditMinimalTest = do
        HUnit.testCase "Create image edit - minimal" do
          _ <-
            createImageEdit
              CreateImageEdit
                { image = "tasty/data/v1/images/image.png",
                  prompt = "The panda should be greener",
                  mask = Nothing,
                  model = Nothing,
                  n = Nothing,
                  size = Nothing,
                  response_format = Nothing,
                  user = Nothing
                }

          return ()

  let createImageEditMaximalTest = do
        HUnit.testCase "Create image edit - maximal" do
          _ <-
            createImageEdit
              CreateImageEdit
                { image = "tasty/data/v1/images/image.png",
                  prompt = "The panda should be greener",
                  mask = Nothing,
                  model = Just "dall-e-2",
                  n = Just 1,
                  size = Just "1024x1024",
                  response_format = Just ResponseFormat.URL,
                  user = Just user
                }

          return ()

  let createImageVariationMinimalTest = do
        HUnit.testCase "Create image variation - minimal" do
          _ <-
            createImageVariation
              CreateImageVariation
                { image = "tasty/data/v1/images/image.png",
                  model = Nothing,
                  n = Nothing,
                  response_format = Nothing,
                  size = Nothing,
                  user = Nothing
                }

          return ()

  let createImageVariationMaximalTest = do
        HUnit.testCase "Create image variation - maximal" do
          _ <-
            createImageVariation
              CreateImageVariation
                { image = "tasty/data/v1/images/image.png",
                  model = Just "dall-e-2",
                  n = Just 1,
                  response_format = Just ResponseFormat.URL,
                  size = Just "1024x1024",
                  user = Just user
                }

          return ()

  let createModerationTest = do
        HUnit.testCase "Create moderation" do
          _ <-
            createModeration
              CreateModeration
                { input = "I am going to kill you",
                  model = Nothing
                }

          return ()

  let assistantsTest = do
        HUnit.testCase "Assistant operations" do
          AssistantObject {id} <-
            createAssistant
              CreateAssistant
                { model = chatModel,
                  name = Nothing,
                  description = Nothing,
                  instructions = Nothing,
                  tools = Nothing,
                  tool_resources = Nothing,
                  metadata = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  response_format = Nothing
                }

          _ <- listAssistants Nothing Nothing Nothing Nothing

          _ <- retrieveAssistant id

          _ <-
            modifyAssistant
              id
              ModifyAssistant
                { model = chatModel,
                  name = Nothing,
                  description = Nothing,
                  instructions = Nothing,
                  tools = Nothing,
                  tool_resources = Nothing,
                  metadata = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  response_format = Nothing
                }

          _ <- deleteAssistant id

          return ()

  let assistantsWithCodeInterpreterTest = do
        HUnit.testCase "Assistant with code interpreter tool" do
          -- Create an assistant enabling the code interpreter tool (no explicit container)
          AssistantObject {id = aid} <-
            createAssistant
              CreateAssistant
                { model = chatModel,
                  name = Nothing,
                  description = Nothing,
                  instructions = Nothing,
                  tools = Just [Tool.codeInterpreter],
                  tool_resources = Nothing,
                  metadata = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  response_format = Nothing
                }

          -- Fetch and then delete to ensure the round trip works
          _ <- retrieveAssistant aid
          _ <- deleteAssistant aid
          return ()

  let messagesTest = do
        HUnit.testCase "Message operations" do
          ThreadObject {id = threadId} <-
            createThread
              Thread
                { messages =
                    Just
                      [ User
                          { content = ["Hi, how can I help you!"],
                            attachments = Nothing,
                            metadata = Nothing
                          }
                      ],
                  tool_resources = Nothing,
                  metadata = Nothing
                }

          MessageObject {id = messageId} <-
            createMessage
              threadId
              User
                { content = ["What is the capital of France?"],
                  attachments = Nothing,
                  metadata = Nothing
                }

          _ <- retrieveMessage threadId messageId

          _ <-
            modifyMessage
              threadId
              messageId
              ModifyMessage
                { metadata = Nothing
                }

          _ <- deleteMessage threadId messageId

          _ <- deleteThread threadId

          return ()

  let waitForRunCompletion :: ThreadID -> RunID -> IO ()
      waitForRunCompletion threadId runId = do
        RunObject {status} <- retrieveRun threadId runId
        case status of
          In_Progress -> do
            Concurrent.threadDelay 1000000 -- Wait 1 second
            waitForRunCompletion threadId runId
          Queued -> do
            Concurrent.threadDelay 1000000 -- Wait 1 second
            waitForRunCompletion threadId runId
          _ -> return () -- Run is completed, failed, cancelled, etc.
  let threadsRunsStepsTest = do
        HUnit.testCase "Thread/Run/Step operations" do
          ThreadObject {id = threadId} <-
            createThread
              Thread
                { messages =
                    Just
                      [ User
                          { content = ["Hello, world!"],
                            attachments = Nothing,
                            metadata = Nothing
                          }
                      ],
                  tool_resources = Nothing,
                  metadata = Nothing
                }

          _ <- retrieveThread threadId

          _ <-
            modifyThread
              threadId
              ModifyThread
                { tool_resources = Nothing,
                  metadata = Nothing
                }

          AssistantObject {id = assistantId} <-
            createAssistant
              CreateAssistant
                { model = chatModel,
                  name = Nothing,
                  description = Nothing,
                  instructions = Nothing,
                  tools = Nothing,
                  tool_resources = Nothing,
                  metadata = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  response_format = Nothing
                }

          RunObject {id = runId} <-
            createRun
              threadId
              Nothing
              CreateRun
                { assistant_id = assistantId,
                  model = Nothing,
                  instructions = Nothing,
                  additional_instructions = Nothing,
                  additional_messages = Nothing,
                  tools = Nothing,
                  metadata = Nothing,
                  temperature = Nothing,
                  top_p = Nothing,
                  max_prompt_tokens = Nothing,
                  max_completion_tokens = Nothing,
                  truncation_strategy = Nothing,
                  tool_choice = Nothing,
                  parallel_tool_calls = Nothing,
                  response_format = Nothing
                }

          _ <- listRuns threadId Nothing Nothing Nothing Nothing

          _ <- retrieveRun threadId runId

          -- Wait for the run to complete before trying to modify it
          waitForRunCompletion threadId runId

          _ <-
            modifyRun
              threadId
              runId
              ModifyRun
                { metadata = Nothing
                }

          _ <- deleteThread threadId

          return ()

  let vectorStoreFilesTest = do
        HUnit.testCase "Vector store file and batch operations" do
          FileObject {id = fileId} <-
            uploadFile
              UploadFile
                { file = "tasty/data/v1/vector_stores/index.html",
                  purpose = Files.Assistants
                }

          VectorStoreObject {id = vectorStoreId} <-
            createVectorStore
              CreateVectorStore
                { file_ids = [],
                  name = Nothing,
                  expires_after = Nothing,
                  chunking_strategy = Nothing,
                  metadata = Nothing
                }

          VectorStoreFileObject {id = vectorStoreFileId} <-
            createVectorStoreFile
              vectorStoreId
              CreateVectorStoreFile
                { file_id = fileId,
                  chunking_strategy = Nothing
                }

          -- Try to create vector store file batch, but handle size limit error gracefully
          batchResult <-
            ( Right
                <$> createVectorStoreFileBatch
                  vectorStoreId
                  CreateVectorStoreFileBatch
                    { file_ids = [fileId],
                      chunking_strategy = Nothing
                    }
              )
              `catch` \(e :: SomeException) -> return (Left e)

          case batchResult of
            Right (VectorStoreFilesBatchObject {id = batchId}) -> do
              _ <- listVectorStores Nothing Nothing Nothing Nothing

              _ <- listVectorStoreFiles vectorStoreId Nothing Nothing Nothing Nothing Nothing

              _ <- listVectorStoreFilesInABatch vectorStoreId batchId Nothing Nothing Nothing Nothing Nothing

              _ <- retrieveVectorStore vectorStoreId

              _ <- retrieveVectorStoreFile vectorStoreId vectorStoreFileId

              _ <- retrieveVectorStoreFileBatch vectorStoreId batchId

              _ <-
                modifyVectorStore
                  vectorStoreId
                  ModifyVectorStore
                    { name = Nothing,
                      expires_after = Nothing,
                      metadata = Nothing
                    }

              _ <- cancelVectorStoreFileBatch vectorStoreId batchId

              _ <- deleteVectorStoreFile vectorStoreId vectorStoreFileId

              _ <- deleteVectorStore vectorStoreId

              _ <- deleteFile fileId

              return ()
            Left _ -> do
              -- If batch creation fails (likely due to size limit), still test other operations
              _ <- listVectorStores Nothing Nothing Nothing Nothing

              _ <- listVectorStoreFiles vectorStoreId Nothing Nothing Nothing Nothing Nothing

              _ <- retrieveVectorStore vectorStoreId

              _ <- retrieveVectorStoreFile vectorStoreId vectorStoreFileId

              _ <-
                modifyVectorStore
                  vectorStoreId
                  ModifyVectorStore
                    { name = Nothing,
                      expires_after = Nothing,
                      metadata = Nothing
                    }

              _ <- deleteVectorStoreFile vectorStoreId vectorStoreFileId

              _ <- deleteVectorStore vectorStoreId

              _ <- deleteFile fileId

              return ()

  let responsesMinimalTest =
        HUnit.testCase "Responses - minimal" do
          _ <-
            createResponse
              Responses._CreateResponse
                { Responses.model = chatModel,
                  Responses.input = Just (Responses.Input
                    [ Responses.Item_Input_Message
                        { Responses.role = Responses.User
                        , Responses.content = [ Responses.Input_Text{ Responses.text = "Say hello in one sentence." } ]
                        , Responses.status = Nothing
                        }
                    ]),
                  Responses.include = Nothing,
                  Responses.parallel_tool_calls = Nothing,
                  Responses.store = Nothing,
                  Responses.instructions = Nothing,
                  Responses.stream = Nothing,
                  Responses.stream_options = Nothing,
                  Responses.metadata = Nothing,
                  Responses.temperature = Nothing,
                  Responses.top_p = Nothing,
                  Responses.tools = Nothing,
                  Responses.tool_choice = Nothing
                }

          return ()

  let responsesStreamingHaikuTest =
        HUnit.testCase "Responses - streaming haiku" do
          let req =
                Responses._CreateResponse
                  { Responses.model = chatModel,
                    Responses.input = Just (Responses.Input
                      [ Responses.Item_Input_Message
                          { Responses.role = Responses.User
                          , Responses.content = [ Responses.Input_Text{ Responses.text = "Stream a short haiku about the sea." } ]
                          , Responses.status = Nothing
                          }
                      ]),
                    Responses.include = Nothing,
                    Responses.parallel_tool_calls = Nothing,
                    Responses.store = Nothing,
                    Responses.instructions = Nothing,
                    Responses.stream = Nothing,
                    Responses.stream_options = Nothing,
                    Responses.metadata = Nothing,
                    Responses.temperature = Nothing,
                    Responses.top_p = Nothing,
                    Responses.tools = Nothing,
                    Responses.tool_choice = Nothing
                  }

          acc <- IORef.newIORef (Text.empty)
          done <- Concurrent.newEmptyMVar

          let onEvent (Left _err) = Concurrent.putMVar done ()
              onEvent (Right ev) = case ev of
                Responses.ResponseTextDeltaEvent{ Responses.delta = d } ->
                  IORef.modifyIORef' acc (<> d)
                Responses.ResponseCompletedEvent{} ->
                  Concurrent.putMVar done ()
                _ -> pure ()

          createResponseStreamTyped req onEvent

          _ <- Concurrent.takeMVar done
          text <- IORef.readIORef acc
          HUnit.assertBool "Expected non-empty streamed text" (not (Text.null text))

          return ()

  let responsesCodeInterpreterStreamingTest =
        HUnit.testCase "Responses - streaming with code interpreter" do
          let req =
                Responses._CreateResponse
                  { Responses.model = chatModel,
                    Responses.input = Just (Responses.Input
                      [ Responses.Item_Input_Message
                          { Responses.role = Responses.User
                          , Responses.content = [ Responses.Input_Text{ Responses.text = "Solve 3x + 11 = 14 and provide x as a number. Use the code interpreter." } ]
                          , Responses.status = Nothing
                          }
                      ]),
                    Responses.include = Nothing,
                    Responses.parallel_tool_calls = Nothing,
                    Responses.store = Nothing,
                    Responses.instructions = Just "You are a math tutor. Use the code interpreter (python) tool to calculate answers when asked about math.",
                    Responses.stream = Nothing,
                    Responses.stream_options = Nothing,
                    Responses.metadata = Nothing,
                    Responses.temperature = Nothing,
                    Responses.top_p = Nothing,
                    Responses.tools = Just
                      [ Responses.Tool_Code_Interpreter
                          { container = Just CodeInterpreterContainer_Auto
                              { file_ids = Nothing
                              }
                          }
                      ],
                    Responses.tool_choice = Just Tool.ToolChoiceRequired
                  }

          acc <- IORef.newIORef (Text.empty)
          sawCI <- IORef.newIORef False
          done <- Concurrent.newEmptyMVar

          let onEvent (Left _err) = Concurrent.putMVar done ()
              onEvent (Right ev) = case ev of
                Responses.ResponseTextDeltaEvent{ Responses.delta = d } ->
                  IORef.modifyIORef' acc (<> d)
                Responses.ResponseCodeInterpreterCallInProgressEvent{} ->
                  IORef.writeIORef sawCI True
                Responses.ResponseCodeInterpreterCallInterpretingEvent{} ->
                  IORef.writeIORef sawCI True
                Responses.ResponseCodeInterpreterCallCompletedEvent{} -> do
                  IORef.writeIORef sawCI True
                Responses.ResponseCompletedEvent{} ->
                  Concurrent.putMVar done ()
                _ -> pure ()

          createResponseStreamTyped req onEvent

          _ <- Concurrent.takeMVar done
          text <- IORef.readIORef acc
          usedCI <- IORef.readIORef sawCI
          HUnit.assertBool "Expected some streamed text" (not (Text.null text))
          HUnit.assertBool "Expected code interpreter activity in stream" usedCI

          return ()

  let responsesReasoningInputTest =
        HUnit.testCase "Responses - reasoning input" do
          response <-
            createResponse
              Responses._CreateResponse
                { Responses.model = reasoningModel
                , Responses.input = Just (Responses.Input
                    [ Responses.Item_Input_Message
                        { Responses.role = Responses.User
                        , Responses.content =
                            [ Responses.Input_Text
                                { Responses.text = "In one sentence, explain why 2 + 2 = 4." }
                            ]
                        , Responses.status = Nothing
                        }
                    ])
                , Responses.reasoning = Just Responses._Reasoning
                    { Responses.effort = Just Responses.ReasoningEffort_None }
                }

          let Responses.ResponseObject
                { Responses.output = responseOutput
                , Responses.reasoning = responseReasoning
                } = response
              responseTexts =
                [ responseText
                | Responses.Item_OutputMessage{ Responses.message_content } <- toList responseOutput
                , Responses.Output_Text{ Responses.text = responseText } <- toList message_content
                ]

          HUnit.assertBool "Expected non-empty response text" (not (Prelude.null responseTexts))
          case responseReasoning of
            Nothing ->
              HUnit.assertFailure "Response missing reasoning metadata"
            Just reasoningConfig ->
              HUnit.assertEqual
                "Reasoning effort not echoed as none"
                (Just Responses.ReasoningEffort_None)
                (Responses.effort reasoningConfig)

  let responsesVerbosityTest =
        HUnit.testCase "Responses - verbosity" do
          response <-
            createResponse
              Responses._CreateResponse
                { Responses.model = reasoningModel
                , Responses.input = Just (Responses.Input
                    [ Responses.Item_Input_Message
                        { Responses.role = Responses.User
                        , Responses.content =
                            [ Responses.Input_Text
                                { Responses.text = "Provide two concise sentences about why daily walking is healthy." }
                            ]
                        , Responses.status = Nothing
                        }
                    ])
                , Responses.text = Just Responses._TextConfig
                    { Responses.verbosity = Just Responses.Verbosity_Medium }
                }

          let Responses.ResponseObject
                { Responses.output = responseOutput
                , Responses.text = responseTextConfig
                , Responses.output_text = responseOutputText
                } = response
              fallbackText = listToMaybe
                [ responseText
                | Responses.Item_OutputMessage{ Responses.message_content } <- toList responseOutput
                , Responses.Output_Text{ Responses.text = responseText } <- toList message_content
                ]

          case responseTextConfig of
            Nothing ->
              HUnit.assertFailure "Response missing text configuration"
            Just cfg ->
              HUnit.assertEqual
                "Verbosity not echoed in response"
                (Just Responses.Verbosity_Medium)
                (Responses.verbosity cfg)

          let finalText = case responseOutputText of
                Just t -> Just t
                Nothing -> fallbackText

          case finalText of
            Nothing ->
              HUnit.assertFailure "Expected non-empty output text"
            Just t ->
              HUnit.assertBool "Expected non-empty output text" (not (Text.null t))

  let responsesTextJSONSchemaTest =
        HUnit.testCase "Responses - text format json_schema" do
          let schemaObject =
                Aeson.object
                  [ "type" .= ("object" :: Text.Text)
                  , "properties"
                      .= Aeson.object
                          [ "city" .= Aeson.object [ "type" .= ("string" :: Text.Text) ]
                          ]
                  , "required" .= (["city"] :: [Text.Text])
                  , "additionalProperties" .= False
                  ]
              request =
                Responses._CreateResponse
                  { Responses.model = chatModel
                  , Responses.input =
                      Just
                        (Responses.Input
                          [ Responses.Item_Input_Message
                              { Responses.role = Responses.User
                              , Responses.content =
                                  [ Responses.Input_Text
                                      { Responses.text = "Return JSON for city=Paris." }
                                  ]
                              , Responses.status = Nothing
                              }
                          ])
                  , Responses.text =
                      Just Responses._TextConfig
                        { Responses.format =
                            Responses.TextFormat_JSON_Schema
                              { Responses.description = Just "Weather response"
                              , Responses.name = "weather_response"
                              , Responses.schema = Just schemaObject
                              , Responses.strict = Just True
                              }
                        }
                  }

          response <- createResponse request

          let Responses.ResponseObject{ Responses.output = responseOutput } = response
              responseTexts =
                [ responseText
                | Responses.Item_OutputMessage{ Responses.message_content } <- toList responseOutput
                , Responses.Output_Text{ Responses.text = responseText } <- toList message_content
                ]

          case listToMaybe responseTexts of
            Nothing ->
              HUnit.assertFailure "Expected at least one text output"
            Just responseText ->
              case Aeson.decodeStrict' (Text.Encoding.encodeUtf8 responseText) of
                Nothing ->
                  HUnit.assertFailure "Expected JSON text output"
                Just (Aeson.Object object) -> do
                  HUnit.assertEqual "Expected only one key (city)" 1 (KeyMap.size object)
                  case KeyMap.lookup "city" object of
                    Just (Aeson.String city) ->
                      HUnit.assertBool "Expected non-empty city" (not (Text.null city))
                    _ ->
                      HUnit.assertFailure "Expected JSON object with string field \"city\""
                Just _ ->
                  HUnit.assertFailure "Expected JSON object output"

  let tests =
          speechTests
            <> [ transcriptionTest,
               translationTest,
               completionsMinimalTest,
               completionsMinimalReasoningTest,
               completionsMaximalTest,
               chatCompletionStreamingHaikuTest,
               embeddingsTest,
               fineTuningTest,
               batchesTest,
               uploadsTest,
               createImageMaximalTest,
               createImageEditMinimalTest,
               createImageEditMaximalTest,
               createImageVariationMinimalTest,
               createImageVariationMaximalTest,
               createModerationTest,
               responsesMinimalTest,
               responsesReasoningInputTest,
               responsesVerbosityTest,
               responsesTextJSONSchemaTest,
               responsesStreamingHaikuTest,
               responsesCodeInterpreterStreamingTest,
               assistantsTest,
               assistantsWithCodeInterpreterTest,
               messagesTest,
               threadsRunsStepsTest,
               vectorStoreFilesTest
             ]

  Tasty.defaultMain (Tasty.testGroup "Tests" tests)
