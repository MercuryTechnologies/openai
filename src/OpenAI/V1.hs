-- | @\/v1@
--
-- Example usage:
--
-- @
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE NamedFieldPuns        #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE OverloadedLists       #-}
--
-- module Main where
--
-- import "Data.Foldable" (`Data.Foldable.traverse_`)
-- import "OpenAI.V1"
-- import "OpenAI.V1.Chat.Completions"
--
-- import qualified "Data.Text" as Text
-- import qualified "Data.Text.IO" as Text.IO
-- import qualified "System.Environment" as Environment
--
-- main :: `IO` ()
-- main = do
--     key <- Environment.`System.Environment.getEnv` \"OPENAI_KEY\"
--
--     clientEnv <- `OpenAI.V1.getClientEnv` \"https://api.openai.com\"
--
--     let `OpenAI.V1.Methods`{ createChatCompletion } = `OpenAI.V1.makeMethods` clientEnv (Text.`Data.Text.pack` key)
--
--     text <- Text.IO.`Data.Text.IO.getLine`
--
--     `OpenAI.V1.Chat.Completions.ChatCompletionObject`{ `OpenAI.V1.Chat.Completions.choices` } <- createChatCompletion `OpenAI.V1.Chat.Completions._CreateChatCompletion`
--         { `OpenAI.V1.Chat.Completions.messages` = [ `OpenAI.V1.Chat.Completions.User`{ `OpenAI.V1.Chat.Completions.content` = [ `OpenAI.V1.Chat.Completions.Text`{ `OpenAI.V1.Chat.Completions.text` } ], `OpenAI.V1.Chat.Completions.name` = `Nothing` } ]
--         , `OpenAI.V1.Chat.Completions.model` = \"gpt-4o-mini\"
--         }
--
--     let display `OpenAI.V1.Chat.Completions.Choice`{ `OpenAI.V1.Chat.Completions.message` } = Text.IO.`Data.Text.IO.putStrLn` (`OpenAI.V1.Chat.Completions.messageToContent` message)
--
--     `Data.Foldable.traverse_` display choices
-- @

module OpenAI.V1
    ( -- * Methods
      getClientEnv
    , makeMethods
    , Methods(..)

      -- * Servant
    , API
    ) where

import Data.ByteString.Char8 ()
import Data.Proxy (Proxy(..))
import OpenAI.Prelude
import OpenAI.V1.Audio.Speech (CreateSpeech)
import OpenAI.V1.Embeddings (CreateEmbeddings, EmbeddingObject)
import OpenAI.V1.Batches (BatchID, BatchObject, CreateBatch)
import OpenAI.V1.DeletionStatus (DeletionStatus)
import OpenAI.V1.Files (FileID, FileObject, UploadFile)
import OpenAI.V1.Images.Image (ImageObject)
import OpenAI.V1.Images.Generations (CreateImage)
import OpenAI.V1.Images.Edits (CreateImageEdit)
import OpenAI.V1.Images.Variations (CreateImageVariation)
import OpenAI.V1.ListOf (ListOf(..))
import OpenAI.V1.Message (Message)
import OpenAI.V1.Models (Model, ModelObject)
import OpenAI.V1.Moderations (CreateModeration, Moderation)
import OpenAI.V1.Order (Order)
import OpenAI.V1.Threads (Thread, ThreadID, ModifyThread, ThreadObject)
import OpenAI.V1.Threads.Runs.Steps (RunStepObject(..), StepID)
import Servant.Client (ClientEnv)
import Servant.Multipart.Client ()

import OpenAI.V1.Assistants
    (AssistantID, AssistantObject, CreateAssistant, ModifyAssistant)
import OpenAI.V1.Audio.Transcriptions
    (CreateTranscription, TranscriptionObject)
import OpenAI.V1.Audio.Translations
    (CreateTranslation, TranslationObject)
import OpenAI.V1.Chat.Completions
    (ChatCompletionObject, CreateChatCompletion)
import OpenAI.V1.Responses
    ( CreateResponse
    , ResponseObject
    , InputItem
    )
import qualified OpenAI.V1.Responses as Responses
import OpenAI.V1.FineTuning.Jobs
    ( CheckpointObject
    , CreateFineTuningJob
    , EventObject
    , FineTuningJobID
    , JobObject
    )
import OpenAI.V1.Threads.Messages
    (MessageID, MessageObject, ModifyMessage)
import OpenAI.V1.Threads.Runs
    ( CreateRun
    , CreateThreadAndRun
    , ModifyRun
    , RunID
    , RunObject
    , SubmitToolOutputsToRun
    )
import OpenAI.V1.Uploads
    ( AddUploadPart
    , CompleteUpload
    , CreateUpload
    , PartObject
    , UploadID
    , UploadObject
    )
import OpenAI.V1.VectorStores
    ( CreateVectorStore(..)
    , ModifyVectorStore(..)
    , VectorStoreID
    , VectorStoreObject(..)
    )
import OpenAI.V1.VectorStores.Files
    ( CreateVectorStoreFile(..)
    , VectorStoreFileID
    , VectorStoreFileObject(..)
    )
import OpenAI.V1.VectorStores.FileBatches
    ( CreateVectorStoreFileBatch(..)
    , VectorStoreFilesBatchObject(..)
    , VectorStoreFileBatchID
    )

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as S8
import Control.Monad (foldM)
import qualified Data.IORef as IORef
import qualified OpenAI.V1.Assistants as Assistants
import qualified OpenAI.V1.Audio as Audio
import qualified OpenAI.V1.Batches as Batches
import qualified OpenAI.V1.Chat.Completions as Chat.Completions
import qualified OpenAI.V1.Embeddings as Embeddings
import qualified OpenAI.V1.FineTuning.Jobs as FineTuning.Jobs
import qualified OpenAI.V1.Files as Files
import qualified OpenAI.V1.Images as Images
import qualified OpenAI.V1.Models as Models
import qualified OpenAI.V1.Moderations as Moderations
import qualified OpenAI.V1.Threads.Runs as Threads.Runs
import qualified OpenAI.V1.Threads.Runs.Steps as Threads.Runs.Steps
import qualified OpenAI.V1.Threads as Threads
import qualified OpenAI.V1.Threads.Messages as Messages
import qualified OpenAI.V1.Uploads as Uploads
import qualified OpenAI.V1.VectorStores as VectorStores
import qualified OpenAI.V1.VectorStores.Files as VectorStores.Files
import qualified OpenAI.V1.VectorStores.FileBatches as VectorStores.FileBatches
import qualified OpenAI.V1.VectorStores.Status as VectorStores.Status
import qualified Servant.Client as Client

-- | Convenient utility to get a `ClientEnv` for the most common use case
getClientEnv
    :: Text
    -- ^ Base URL for API
    -> IO ClientEnv
getClientEnv baseUrlText = do
    baseUrl <- Client.parseBaseUrl (Text.unpack baseUrlText)

    let managerSettings = TLS.tlsManagerSettings
            { HTTP.Client.managerResponseTimeout =
                HTTP.Client.responseTimeoutNone
            }

    manager <- TLS.newTlsManagerWith managerSettings

    pure (Client.mkClientEnv manager baseUrl)

-- | Get a record of API methods after providing an API token
makeMethods
    :: ClientEnv
    -- ^
    -> Text
    -- ^ API token
    -> Maybe Text
    -- ^ Organization ID
    -> Maybe Text
    -- ^ Project ID
    -> Methods
makeMethods clientEnv token organizationID projectID = Methods{..}
  where
    authorization = "Bearer " <> token

    (       (     createSpeech
            :<|>  createTranscription_
            :<|>  createTranslation_
                    )
      :<|>  createChatCompletion
      :<|>  (     createResponse
            :<|>  retrieveResponse
            :<|>  cancelResponse
            :<|>  listResponseInputItems_
            )
      :<|>  createEmbeddings_
      :<|>  (     createFineTuningJob
            :<|>  listFineTuningJobs_
            :<|>  listFineTuningEvents_
            :<|>  listFineTuningCheckpoints_
            :<|>  retrieveFineTuningJob
            :<|>  cancelFineTuning
            )
      :<|>  (     createBatch
            :<|>  retrieveBatch
            :<|>  cancelBatch
            :<|>  listBatch_
            )
      :<|>  (     uploadFile_
            :<|>  listFiles_
            :<|>  retrieveFile
            :<|>  deleteFile
            :<|>  retrieveFileContent
            )
      :<|>  (     createImage_
            :<|>  createImageEdit_
            :<|>  createImageVariation_
            )
      :<|>  (     createUpload
            :<|>  addUploadPart_
            :<|>  completeUpload
            :<|>  cancelUpload
            )
      :<|>  (     listModels_
            :<|>  retrieveModel
            :<|>  deleteModel
            )
      :<|>  (     createModeration
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createAssistant
                :<|>  listAssistants_
                :<|>  retrieveAssistant
                :<|>  modifyAssistant
                :<|>  deleteAssistant
                )
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createThread
                :<|>  retrieveThread
                :<|>  modifyThread
                :<|>  deleteThread
                )
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createMessage
                :<|>  listMessages_
                :<|>  retrieveMessage
                :<|>  modifyMessage
                :<|>  deleteMessage
                )
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createRun
                :<|>  createThreadAndRun
                :<|>  listRuns_
                :<|>  retrieveRun
                :<|>  modifyRun
                :<|>  submitToolOutputsToRun
                :<|>  cancelRun
                )
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     listRunSteps_
                :<|>  retrieveRunStep
                )
            )
      
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createVectorStore
                :<|>  listVectorStores_
                :<|>  retrieveVectorStore
                :<|>  modifyVectorStore
                :<|>  deleteVectorStore
                )
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createVectorStoreFile
                :<|>  listVectorStoreFiles_
                :<|>  retrieveVectorStoreFile
                :<|>  deleteVectorStoreFile
                )
            )
      :<|>  (   (\x -> x "assistants=v2")
            ->  (     createVectorStoreFileBatch
                :<|>  retrieveVectorStoreFileBatch
                :<|>  cancelVectorStoreFileBatch
                :<|>  listVectorStoreFilesInABatch_
                )
            )
      ) = Client.hoistClient @API Proxy run (Client.client @API Proxy) authorization organizationID projectID

    run :: Client.ClientM a -> IO a
    run clientM = do
        result <- Client.runClientM clientM clientEnv
        case result of
            Left exception -> Exception.throwIO exception
            Right a -> return a

    toVector :: IO (ListOf a) -> IO (Vector a)
    toVector = fmap adapt
      where
        adapt List{ data_ } = data_

    createTranscription a = createTranscription_ (boundary, a)
    createTranslation a = createTranslation_ (boundary, a)
    createEmbeddings a = toVector (createEmbeddings_ a)
    listFineTuningJobs a b = toVector (listFineTuningJobs_ a b)
    listFineTuningEvents a b c = toVector (listFineTuningEvents_ a b c)
    listFineTuningCheckpoints a b c =
        toVector (listFineTuningCheckpoints_ a b c)
    listBatch a b = toVector (listBatch_ a b)
    uploadFile a = uploadFile_ (boundary, a)
    listFiles a b c d = toVector (listFiles_ a b c d)
    addUploadPart a b = addUploadPart_ a (boundary, b)
    createImage a = toVector (createImage_ a)
    createImageEdit a = toVector (createImageEdit_ (boundary, a))
    createImageVariation a = toVector (createImageVariation_ (boundary, a))
    listModels = toVector listModels_
    listAssistants a b c d = toVector (listAssistants_ a b c d)
    listMessages a = toVector (listMessages_ a)
    listRuns a b c d e = toVector (listRuns_ a b c d e)
    listRunSteps a b c d e f g = toVector (listRunSteps_ a b c d e f g)
    listVectorStores a b c d = toVector (listVectorStores_ a b c d)
    listVectorStoreFiles a b c d e f =
        toVector (listVectorStoreFiles_ a b c d e f)
    listVectorStoreFilesInABatch a b c d e f g =
        toVector (listVectorStoreFilesInABatch_ a b c d e f g)
    listResponseInputItems a = toVector (listResponseInputItems_ a)

    -- Streaming implementation using http-client and SSE parsing
    createResponseStream req onEvent = do
        let req' = req{ Responses.stream = Just True }
        ssePostJSON "/v1/responses" req' onEvent

    createResponseStreamTyped
        :: CreateResponse
        -> (Either Text Responses.ResponseStreamEvent -> IO ())
        -> IO ()
    createResponseStreamTyped req onEvent =
        createResponseStream req $ \ev -> case ev of
            Left err -> onEvent (Left err)
            Right val -> case Aeson.fromJSON val of
                Aeson.Error msg -> onEvent (Left (Text.pack msg))
                Aeson.Success e -> onEvent (Right e)

    ssePostJSON :: ToJSON a
                => String
                -> a
                -> (Either Text Aeson.Value -> IO ())
                -> IO ()
    ssePostJSON path body onEvent = do
        let base = Client.baseUrl clientEnv
        let secure = case Client.baseUrlScheme base of
                Client.Http -> False
                Client.Https -> True
        let host = S8.pack (Client.baseUrlHost base)
        let port = Client.baseUrlPort base
        let basePath = Client.baseUrlPath base
        let fullPath = S8.pack (normalizePath basePath <> path)

        let headers0 =
                [ ("Authorization", S8.pack (Text.unpack authorization))
                , ("Accept", "text/event-stream")
                , ("Content-Type", "application/json")
                ]
        let headers1 = case organizationID of
                Nothing -> headers0
                Just org -> ("OpenAI-Organization", S8.pack (Text.unpack org)) : headers0
        let headers = case projectID of
                Nothing -> headers1
                Just proj -> ("OpenAI-Project", S8.pack (Text.unpack proj)) : headers1

        let request = HTTP.Client.defaultRequest
                { HTTP.Client.secure = secure
                , HTTP.Client.host = host
                , HTTP.Client.port = port
                , HTTP.Client.method = "POST"
                , HTTP.Client.path = fullPath
                , HTTP.Client.requestHeaders = headers
                , HTTP.Client.requestBody = HTTP.Client.RequestBodyLBS (Aeson.encode body)
                , HTTP.Client.responseTimeout = HTTP.Client.responseTimeoutNone
                }

        HTTP.Client.withResponse request (Client.manager clientEnv) $ \response -> do
            let br = HTTP.Client.responseBody response
            lineBufRef <- IORef.newIORef SBS.empty
            eventBufRef <- IORef.newIORef ([] :: [SBS.ByteString])
            let flushEvent = do
                    es <- fmap reverse (IORef.readIORef eventBufRef)
                    IORef.writeIORef eventBufRef []
                    if null es
                        then pure False
                        else do
                            let payload = S8.intercalate "\n" es
                            if payload == "[DONE]"
                                then pure True
                                else case (Aeson.eitherDecodeStrict payload :: Either String Aeson.Value) of
                                    Left err -> onEvent (Left (Text.pack err)) >> pure False
                                    Right val -> onEvent (Right val) >> pure False

            let handleLine line = do
                    let l = stripCR line
                    if S8.null l
                        then flushEvent
                        else if "data:" `S8.isPrefixOf` l
                            then do
                                let d = S8.dropWhile (==' ') (S8.drop 5 l)
                                IORef.modifyIORef' eventBufRef (d:)
                                pure False
                            else pure False

            let loop = do
                    chunk <- HTTP.Client.brRead br
                    if SBS.null chunk
                        then do
                            -- flush any pending event at EOF
                            _ <- flushEvent
                            pure ()
                        else do
                            prev <- IORef.readIORef lineBufRef
                            let combined = prev <> chunk
                            let ls = S8.split '\n' combined
                            case unsnoc ls of
                                Nothing -> loop
                                Just (completeLines, lastLine) -> do
                                    IORef.writeIORef lineBufRef lastLine
                                    stop <- foldM (\acc ln -> if acc then pure True else handleLine ln) False completeLines
                                    if stop then pure () else loop

            loop

    normalizePath p = case p of
        "" -> ""
        ('/':_) -> p
        _ -> '/':p

    stripCR bs = case S8.unsnoc bs of
        Just (initBs, '\r') -> initBs
        _ -> bs

    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)

-- | Hard-coded boundary to simplify the user-experience
--
-- I don't understand why `multipart-servant-client` insists on generating a
-- fresh boundary for each request (or why it doesn't handle that for you)
boundary :: ByteString
boundary = "j3qdD3XtDVjvva8IIqoBzHQAYwCenObtPMkxAFnylwFyU5xffWKoYrY"

-- | API methods
data Methods = Methods
    { createSpeech :: CreateSpeech -> IO ByteString
    , createTranscription :: CreateTranscription -> IO TranscriptionObject
    , createTranslation :: CreateTranslation -> IO TranslationObject
    , createChatCompletion :: CreateChatCompletion -> IO ChatCompletionObject
    , createResponse :: CreateResponse -> IO ResponseObject
    , createResponseStream
        :: CreateResponse
        -> (Either Text Aeson.Value -> IO ())
        -> IO ()
    , createEmbeddings :: CreateEmbeddings -> IO (Vector EmbeddingObject)
    , createResponseStreamTyped
        :: CreateResponse
        -> (Either Text Responses.ResponseStreamEvent -> IO ())
        -> IO ()
    , createFineTuningJob :: CreateFineTuningJob -> IO JobObject
    , listFineTuningJobs
        :: Maybe Text
        -- ^ after
        -> Maybe Natural
        -- ^ limit
        -> IO (Vector JobObject)
    , listFineTuningEvents
        :: FineTuningJobID
        -- ^
        -> Maybe Text
        -- ^ after
        -> Maybe Natural
        -- ^ limit
        -> IO (Vector EventObject)
    , listFineTuningCheckpoints
        :: FineTuningJobID
        -- ^
        -> Maybe Text
        -- ^ after
        -> Maybe Natural
        -- ^ limit
        -> IO (Vector CheckpointObject)
    , retrieveFineTuningJob :: FineTuningJobID -> IO JobObject
    , cancelFineTuning :: FineTuningJobID -> IO JobObject
    , createBatch :: CreateBatch -> IO BatchObject
    , retrieveBatch :: BatchID -> IO BatchObject
    , cancelBatch :: BatchID -> IO BatchObject
    , listBatch
        :: Maybe Text
        -- ^ after
        -> Maybe Natural
        -- ^ limit
        -> IO (Vector BatchObject)
    , uploadFile :: UploadFile -> IO FileObject
    , listFiles
        :: Maybe Files.Purpose
        -- ^ purpose
        -> Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> IO (Vector FileObject)
    , retrieveFile :: FileID -> IO FileObject
    , deleteFile :: FileID -> IO DeletionStatus
    , retrieveFileContent :: FileID -> IO ByteString
    , createUpload
        :: CreateUpload -> IO (UploadObject (Maybe Void))
    , addUploadPart :: UploadID -> AddUploadPart -> IO PartObject
    , completeUpload
        :: UploadID -> CompleteUpload -> IO (UploadObject FileObject)
    , cancelUpload :: UploadID -> IO (UploadObject (Maybe Void))
    , createImage :: CreateImage -> IO (Vector ImageObject)
    , createImageEdit :: CreateImageEdit -> IO (Vector ImageObject)
    , createImageVariation :: CreateImageVariation -> IO (Vector ImageObject)
    , listModels :: IO (Vector ModelObject)
    , retrieveModel :: Model -> IO ModelObject
    , deleteModel :: Model -> IO DeletionStatus
    , createModeration :: CreateModeration -> IO Moderation
    , createAssistant :: CreateAssistant -> IO AssistantObject
    , listAssistants
        :: Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> Maybe Text
        -- ^ before
        -> IO (Vector AssistantObject)
    , retrieveAssistant :: AssistantID -> IO AssistantObject
    , modifyAssistant :: AssistantID -> ModifyAssistant -> IO AssistantObject
    , deleteAssistant :: AssistantID -> IO DeletionStatus
    , createThread :: Thread -> IO ThreadObject
    , retrieveThread :: ThreadID -> IO ThreadObject
    , modifyThread :: ThreadID -> ModifyThread -> IO ThreadObject
    , deleteThread :: ThreadID -> IO DeletionStatus
    , createMessage :: ThreadID -> Message -> IO MessageObject
    , listMessages :: ThreadID -> IO (Vector MessageObject)
    , retrieveMessage :: ThreadID -> MessageID -> IO MessageObject
    , modifyMessage
        :: ThreadID -> MessageID -> ModifyMessage -> IO MessageObject
    , deleteMessage :: ThreadID -> MessageID -> IO DeletionStatus
    , createRun
        :: ThreadID
        -- ^
        -> Maybe Text
        -- ^ include[]
        -> CreateRun
        -- ^
        -> IO RunObject
    , createThreadAndRun :: CreateThreadAndRun -> IO RunObject
    , listRuns
        :: ThreadID
        -- ^
        -> Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> Maybe Text
        -- ^ before
        -> IO (Vector RunObject)
    , retrieveRun :: ThreadID -> RunID -> IO RunObject
    , modifyRun :: ThreadID -> RunID -> ModifyRun -> IO RunObject
    , submitToolOutputsToRun
        :: ThreadID -> RunID -> SubmitToolOutputsToRun -> IO RunObject
    , cancelRun :: ThreadID -> RunID -> IO RunObject
    , listRunSteps
        :: ThreadID
        -- ^
        -> RunID
        -- ^
        -> Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> Maybe Text
        -- ^ before
        -> Maybe Text
        -- ^ include[]
        -> IO (Vector RunStepObject)
    , retrieveRunStep
        :: ThreadID
        -- ^
        -> RunID
        -- ^
        -> StepID
        -- ^
        -> Maybe Text
        -- ^ include[]
        -> IO RunStepObject
    , retrieveResponse :: Text -> IO ResponseObject
    , cancelResponse :: Text -> IO ResponseObject
    , listResponseInputItems :: Text -> IO (Vector InputItem)
    , createVectorStore :: CreateVectorStore -> IO VectorStoreObject
    , listVectorStores
        :: Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> Maybe Text
        -- ^ before
        -> IO (Vector VectorStoreObject)
    , retrieveVectorStore :: VectorStoreID -> IO VectorStoreObject
    , modifyVectorStore
        :: VectorStoreID -> ModifyVectorStore -> IO VectorStoreObject
    , deleteVectorStore :: VectorStoreID -> IO DeletionStatus
    , createVectorStoreFile
        :: VectorStoreID -> CreateVectorStoreFile -> IO VectorStoreFileObject
    , listVectorStoreFiles
        :: VectorStoreID
        -- ^
        -> Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> Maybe Text
        -- ^ before
        -> Maybe VectorStores.Status.Status
        -- ^ filter
        -> IO (Vector VectorStoreFileObject)
    , retrieveVectorStoreFile
        :: VectorStoreID -> VectorStoreFileID -> IO VectorStoreFileObject
    , deleteVectorStoreFile
        :: VectorStoreID -> VectorStoreFileID -> IO DeletionStatus
    , createVectorStoreFileBatch
        :: VectorStoreID
        -> CreateVectorStoreFileBatch
        -> IO VectorStoreFilesBatchObject
    , retrieveVectorStoreFileBatch
        :: VectorStoreID
        -> VectorStoreFileBatchID
        -> IO VectorStoreFilesBatchObject
    , cancelVectorStoreFileBatch
        :: VectorStoreID
        -> VectorStoreFileBatchID
        -> IO VectorStoreFilesBatchObject
    , listVectorStoreFilesInABatch
        :: VectorStoreID
        -- ^
        -> VectorStoreFileBatchID
        -- ^
        -> Maybe Natural
        -- ^ limit
        -> Maybe Order
        -- ^ order
        -> Maybe Text
        -- ^ after
        -> Maybe Text
        -- ^ before
        -> Maybe VectorStores.Status.Status
        -- ^ filter
        -> IO (Vector VectorStoreFilesBatchObject)
    }

-- | Servant API
type API
    =   Header' [ Required, Strict ] "Authorization" Text
    :>  Header' [ Optional, Strict ] "OpenAI-Organization" Text
    :>  Header' [ Optional, Strict ] "OpenAI-Project" Text
    :>  "v1"
    :>  (     Audio.API
        :<|>  Chat.Completions.API
        :<|>  Responses.API
        :<|>  Embeddings.API
        :<|>  FineTuning.Jobs.API
        :<|>  Batches.API
        :<|>  Files.API
        :<|>  Images.API
        :<|>  Uploads.API
        :<|>  Models.API
        :<|>  Moderations.API
        :<|>  Assistants.API
        :<|>  Threads.API
        :<|>  Messages.API
        :<|>  Threads.Runs.API
        :<|>  Threads.Runs.Steps.API
        :<|>  VectorStores.API
        :<|>  VectorStores.Files.API
        :<|>  VectorStores.FileBatches.API
        )
