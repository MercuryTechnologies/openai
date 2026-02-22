-- | Streaming payload types for @/v1/chat/completions@.
module OpenAI.V1.Chat.Completions.Stream
    ( ChatCompletionChunk(..)
    , ChunkChoice(..)
    , Delta(..)
    , ChatCompletionStreamEvent
    ) where

import OpenAI.Prelude
import OpenAI.V1.Chat.Completions (LogProbs, ServiceTier)
import OpenAI.V1.Models (Model)
import OpenAI.V1.ToolCall (ToolCall)
import OpenAI.V1.Usage
    ( CompletionTokensDetails
    , PromptTokensDetails
    , Usage
    )
import Prelude hiding (id)

-- | Delta message content for streaming
data Delta = Delta
    { delta_content :: Maybe Text
    , delta_refusal :: Maybe Text
    , delta_role :: Maybe Text
    , delta_tool_calls :: Maybe (Vector ToolCall)
    } deriving stock (Generic, Show)

deltaOptions :: Options
deltaOptions = aesonOptions
    { fieldLabelModifier = stripPrefix "delta_"
    }

instance FromJSON Delta where
    parseJSON = genericParseJSON deltaOptions

instance ToJSON Delta where
    toJSON = genericToJSON deltaOptions

-- | A streaming choice chunk
data ChunkChoice = ChunkChoice
    { delta :: Delta
    , finish_reason :: Maybe Text
    , index :: Natural
    , logprobs :: Maybe LogProbs
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Chat completion chunk (streaming response)
data ChatCompletionChunk = ChatCompletionChunk
    { id :: Text
    , choices :: Vector ChunkChoice
    , created :: POSIXTime
    , model :: Model
    , service_tier :: Maybe ServiceTier
    , system_fingerprint :: Maybe Text
    , object :: Text
    , usage :: Maybe (Usage CompletionTokensDetails PromptTokensDetails)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Type alias for streaming events (currently just chunks)
type ChatCompletionStreamEvent = ChatCompletionChunk
