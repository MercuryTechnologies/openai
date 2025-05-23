-- | @\/v1\/chat\/completions@
--
-- Streaming results are not yet supported
module OpenAI.V1.Chat.Completions
    ( -- * Main types
      CreateChatCompletion(..)
    , _CreateChatCompletion
    , ChatCompletionObject(..)
    , Choice(..)
    , Message(..)
    , messageToContent
    , Content(..)
      -- * Other types
    , InputAudio(..)
    , ImageURL(..)
    , AudioData(..)
    , Modality(..)
    , Prediction(..)
    , Voice(..)
    , AudioFormat(..)
    , AudioParameters(..)
    , ResponseFormat(..)
    , ServiceTier(..)
    , ReasoningEffort(..)
    , SearchContextSize(..)
    , UserLocation(..)
    , WebSearchOptions(..)
    , FinishReason(..)
    , Token(..)
    , LogProbs(..)
    -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.AutoOr
import OpenAI.V1.Models (Model)
import OpenAI.V1.ResponseFormat
import OpenAI.V1.Tool
import OpenAI.V1.ToolCall
import OpenAI.V1.Usage
import Prelude hiding (id)

-- | Audio content part
data InputAudio = InputAudio{ data_ :: Text, format :: AudioFormat }
    deriving stock (Generic, Show)

instance FromJSON InputAudio where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InputAudio where
    toJSON = genericToJSON aesonOptions

-- | Image content part
data ImageURL = ImageURL{ url :: Text, detail :: Maybe (AutoOr Text) }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | A content part
data Content
    = Text{ text :: Text }
    | Image_URL{ image_url :: ImageURL }
    | Input_Audio{ input_audio :: InputAudio }
    deriving (Generic, Show)

instance IsString Content where
    fromString string = Text{ text = fromString string }

contentOptions :: Options
contentOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON Content where
    parseJSON = genericParseJSON contentOptions

instance ToJSON Content where
    toJSON = genericToJSON contentOptions

-- | Data about a previous audio response from the model.
-- [Learn more](https://platform.openai.com/docs/guides/audio)
data AudioData = AudioData{ id :: Text }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | A message from the conversation so far
data Message content
    = System
        { content :: content
        , name :: Maybe Text
        }
    | User
        { content :: content
        , name :: Maybe Text
        }
    | Assistant
        { assistant_content :: Maybe content
        , refusal :: Maybe Text
        , name :: Maybe Text
        , assistant_audio :: Maybe AudioData
        , tool_calls :: Maybe (Vector ToolCall)
        }
    | Tool
        { content :: content
        , tool_call_id :: Text
        }
    deriving stock (Generic, Show)

messageOptions :: Options
messageOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "role", contentsFieldName = "" }

    , tagSingleConstructors = True

    , fieldLabelModifier = stripPrefix "assistant_"
    }

instance FromJSON content => FromJSON (Message content) where
    parseJSON = genericParseJSON messageOptions

instance ToJSON content => ToJSON (Message content) where
    toJSON = genericToJSON messageOptions

-- | Extract the message body from a `Message`
--
-- Normally this would just be the @content@ field selector, but the problem
-- is that the content field for the `Assistant` constructor is not required
-- to be present, so we provide a utility function to default to extract the
-- @content@ field for all constructors, defaulting to `mempty` for the special
-- case where the `Message` is an `Assistant` constructor with a missing
-- @content@ field
messageToContent :: Monoid content => Message content -> content
messageToContent System{ content } = content
messageToContent User{ content } = content
messageToContent Assistant{ assistant_content = Just content } = content
messageToContent Assistant{ assistant_content = Nothing } = mempty
messageToContent Tool{ content } = content

-- | Output types that you would like the model to generate for this request
data Modality = Modality_Text | Modality_Audio
    deriving stock (Generic, Show)

modalityOptions :: Options
modalityOptions =
    aesonOptions
        { constructorTagModifier = stripPrefix "Modality_" }

instance FromJSON Modality where
    parseJSON = genericParseJSON modalityOptions

instance ToJSON Modality where
    toJSON = genericToJSON modalityOptions

-- | Configuration for a
-- [Predicted Output](https://platform.openai.com/docs/guides/predicted-outputs),
-- which can greatly improve response times when large parts of the model
-- response are known ahead of time. This is most common when you are
-- regenerating a file with only minor changes to most of the content
data Prediction = Content{ content :: Text }
    deriving stock (Generic, Show)

predictionOptions :: Options
predictionOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON Prediction where
    parseJSON = genericParseJSON predictionOptions

instance ToJSON Prediction where
    toJSON = genericToJSON predictionOptions

-- | The voice the model uses to respond
data Voice = Ash | Ballad | Coral | Sage | Verse
    deriving stock (Generic, Show)

instance FromJSON Voice where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Voice where
    toJSON = genericToJSON aesonOptions

-- | Specifies the output audio format
data AudioFormat = WAV | MP3 | FLAC | Opus | PCM16
    deriving stock (Generic, Show)

instance FromJSON AudioFormat where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON AudioFormat where
    toJSON = genericToJSON aesonOptions

-- | Parameters for audio output
data AudioParameters = AudioParameters
    { voice :: Voice
    , format :: AudioFormat
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Specifies the latency tier to use for processing the request
data ServiceTier = Default
    deriving stock (Generic, Show)

serviceOptions :: Options
serviceOptions = aesonOptions{ tagSingleConstructors = True }

instance FromJSON ServiceTier where
    parseJSON = genericParseJSON serviceOptions

instance ToJSON ServiceTier where
    toJSON = genericToJSON serviceOptions

-- | Constrains effort on reasoning for reasoning models
data ReasoningEffort
    = ReasoningEffort_Low
    | ReasoningEffort_Medium
    | ReasoningEffort_High
    deriving stock (Generic, Show)

reasoningEffortOptions :: Options
reasoningEffortOptions =
    aesonOptions
        { constructorTagModifier = stripPrefix "ReasoningEffort_" }

instance FromJSON ReasoningEffort where
    parseJSON = genericParseJSON reasoningEffortOptions

instance ToJSON ReasoningEffort where
    toJSON = genericToJSON reasoningEffortOptions

-- | High level guidance for the amount of context window space to use for the
-- search
data SearchContextSize
    = SearchContextSize_Low
    | SearchContextSize_Medium
    | SearchContextSize_High
    deriving stock (Generic, Show)

instance FromJSON SearchContextSize where
    parseJSON = genericParseJSON searchContextSizeOptions

instance ToJSON SearchContextSize where
    toJSON = genericToJSON searchContextSizeOptions

-- | Approximate location parameters for the search
data UserLocation = Approximate
    { city :: Maybe Text
    , country :: Maybe Text
    , region :: Maybe Text
    , timezone :: Maybe Text
    } deriving stock (Generic, Show)

instance FromJSON UserLocation where
    parseJSON = genericParseJSON userLocationOptions

instance ToJSON UserLocation where
    toJSON = genericToJSON userLocationOptions

userLocationOptions :: Options
userLocationOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

searchContextSizeOptions :: Options
searchContextSizeOptions =
    aesonOptions
        { constructorTagModifier = stripPrefix "SearchContextSize_" }

-- | Search the web for relevant results to use in a response
data WebSearchOptions = WebSearchOptions
    { search_context_size :: Maybe SearchContextSize
    , user_location :: Maybe UserLocation
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/v1\/chat\/completions@
data CreateChatCompletion = CreateChatCompletion
    { messages :: Vector (Message (Vector Content))
    , model :: Model
    , store :: Maybe Bool
    , metadata :: Maybe (Map Text Text)
    , frequency_penalty :: Maybe Double
    , logit_bias :: Maybe (Map Word Int)
    , logprobs :: Maybe Bool
    , top_logprobs :: Maybe Word
    , max_completion_tokens :: Maybe Natural
    , n :: Maybe Natural
    , modalities :: Maybe (Vector Modality)
    , prediction :: Maybe Prediction
    , audio :: Maybe AudioParameters
    , presence_penalty :: Maybe Double
    , reasoning_effort :: Maybe ReasoningEffort
    , response_format :: Maybe ResponseFormat
    , seed :: Maybe Integer
    , service_tier :: Maybe (AutoOr ServiceTier)
    , stop :: Maybe (Vector Text)
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , tools :: Maybe (Vector Tool)
    , tool_choice :: Maybe ToolChoice
    , parallel_tool_calls :: Maybe Bool
    , user :: Maybe Text
    , web_search_options :: Maybe WebSearchOptions
    } deriving stock (Generic, Show)

instance FromJSON CreateChatCompletion where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateChatCompletion where
    toJSON = genericToJSON aesonOptions

-- | Default `CreateChatCompletion`
_CreateChatCompletion :: CreateChatCompletion
_CreateChatCompletion = CreateChatCompletion
    { store = Nothing
    , metadata = Nothing
    , frequency_penalty = Nothing
    , logit_bias = Nothing
    , logprobs = Nothing
    , top_logprobs = Nothing
    , max_completion_tokens = Nothing
    , n = Nothing
    , modalities = Nothing
    , prediction = Nothing
    , audio = Nothing
    , presence_penalty = Nothing
    , reasoning_effort = Nothing
    , response_format = Nothing
    , seed = Nothing
    , service_tier = Nothing
    , stop = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , tools = Nothing
    , tool_choice = Nothing
    , parallel_tool_calls = Nothing
    , user = Nothing
    , web_search_options = Nothing
    }

-- | The reason the model stopped generating tokens
data FinishReason
    = Stop
    | Length
    | Content_Filter
    | Tool_Calls
    deriving stock (Generic, Show)

instance FromJSON FinishReason where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON FinishReason where
    toJSON = genericToJSON aesonOptions

-- | Message tokens with log probability information
data Token = Token
    { token :: Text
    , logprob :: Double
    , bytes :: Maybe (Vector Word8)
    , top_logprobs :: Maybe (Vector Token)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Log probability information for the choice
data LogProbs = LogProbs
    { content :: Maybe (Vector Token)
    , refusal :: Maybe (Vector Token)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A chat completion choice
data Choice = Choice
    { finish_reason :: Text
    , index :: Natural
    , message :: Message Text
    , logprobs :: Maybe LogProbs
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | ChatCompletion body
data ChatCompletionObject = ChatCompletionObject
    { id :: Text
    , choices :: Vector Choice
    , created :: POSIXTime
    , model :: Model
    , reasoning_effort :: Maybe ReasoningEffort
    , service_tier :: Maybe ServiceTier
    , system_fingerprint :: Maybe Text
    , object :: Text
    , usage :: Usage CompletionTokensDetails PromptTokensDetails
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "chat"
    :>  "completions"
    :>  ReqBody '[JSON] CreateChatCompletion
    :>  Post '[JSON] ChatCompletionObject
