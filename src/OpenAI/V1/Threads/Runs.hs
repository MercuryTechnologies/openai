-- | @\/v1\/threads\/:thread_id\/runs@
module OpenAI.V1.Threads.Runs
    ( -- * Main types
      RunID(..)
    , CreateRun(..)
    , _CreateRun
    , CreateThreadAndRun(..)
    , _CreateThreadAndRun
    , ModifyRun(..)
    , _ModifyRun
    , SubmitToolOutputsToRun(..)
    , _SubmitToolOutputsToRun
    , RunObject(..)

      -- * Other types
    , TruncationStrategy(..)
    , SubmitToolOutputs(..)
    , RequiredAction(..)
    , IncompleteDetails(..)
    , ToolOutput(..)
    , Status(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Assistants (AssistantID)
import OpenAI.V1.AutoOr
import OpenAI.V1.Error
import OpenAI.V1.ListOf
import OpenAI.V1.Message
import OpenAI.V1.Models (Model)
import OpenAI.V1.Order
import OpenAI.V1.ResponseFormat
import OpenAI.V1.Threads (Thread)
import OpenAI.V1.Tool
import OpenAI.V1.ToolCall
import OpenAI.V1.ToolResources
import OpenAI.V1.Threads (ThreadID)
import OpenAI.V1.Usage

-- | Run ID
newtype RunID = RunID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Controls for how a thread will be truncated prior to the run
data TruncationStrategy
    = Auto
    | Last_Messages{ last_messages :: Maybe Natural }
    deriving stock (Generic, Show)

truncationStrategyOptions :: Options
truncationStrategyOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True
    }

instance FromJSON TruncationStrategy where
    parseJSON = genericParseJSON truncationStrategyOptions

instance ToJSON TruncationStrategy where
    toJSON = genericToJSON truncationStrategyOptions

-- | Request body for @\/v1\/threads\/:thread_id\/runs@
data CreateRun = CreateRun
    { assistant_id :: AssistantID
    , model :: Maybe Model
    , instructions :: Maybe Text
    , additional_instructions :: Maybe Text
    , additional_messages :: Maybe (Vector Message)
    , tools :: Maybe (Vector Tool)
    , metadata :: Maybe (Map Text Text)
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , max_prompt_tokens :: Maybe Natural
    , max_completion_tokens :: Maybe Natural
    , truncation_strategy :: Maybe TruncationStrategy
    , tool_choice :: Maybe ToolChoice
    , parallel_tool_calls :: Maybe Bool
    , response_format :: Maybe (AutoOr ResponseFormat)
    } deriving stock (Generic, Show)

instance FromJSON CreateRun where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateRun where
    toJSON = genericToJSON aesonOptions

-- | Default `CreateRun`
_CreateRun :: CreateRun
_CreateRun = CreateRun
    { model = Nothing
    , instructions = Nothing
    , additional_instructions = Nothing
    , additional_messages = Nothing
    , tools = Nothing
    , metadata = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , max_prompt_tokens = Nothing
    , max_completion_tokens = Nothing
    , truncation_strategy = Nothing
    , tool_choice = Nothing
    , parallel_tool_calls = Nothing
    , response_format = Nothing
    }

-- | Request body for @\/v1\/threads\/runs@
data CreateThreadAndRun = CreateThreadAndRun
    { assistant_id :: AssistantID
    , thread :: Maybe Thread
    , model :: Maybe Model
    , instructions :: Maybe Text
    , tools :: Maybe (Vector Tool)
    , toolResources :: Maybe ToolResources
    , metadata :: Maybe (Map Text Text)
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , max_prompt_tokens :: Maybe Natural
    , max_completion_tokens :: Maybe Natural
    , truncation_strategy :: Maybe TruncationStrategy
    , tool_choice :: Maybe ToolChoice
    , parallel_tool_calls :: Maybe Bool
    , response_format :: Maybe (AutoOr ResponseFormat)
    } deriving stock (Generic, Show)

instance FromJSON CreateThreadAndRun where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateThreadAndRun where
    toJSON = genericToJSON aesonOptions

-- | Default `CreateThreadAndRun`
_CreateThreadAndRun :: CreateThreadAndRun
_CreateThreadAndRun = CreateThreadAndRun
    { thread = Nothing
    , model = Nothing
    , instructions = Nothing
    , tools = Nothing
    , toolResources = Nothing
    , metadata = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , max_prompt_tokens = Nothing
    , max_completion_tokens = Nothing
    , truncation_strategy = Nothing
    , tool_choice = Nothing
    , parallel_tool_calls = Nothing
    , response_format = Nothing
    }

-- | Details on the tool outputs needed for this run to continue.
data SubmitToolOutputs = SubmitToolOutputs
    { tool_calls :: Vector ToolCall
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The status of the run
data Status
    = Queued
    | In_Progress
    | Requires_Action
    | Cancelling
    | Cancelled
    | Failed
    | Completed
    | Incomplete
    | Expired
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | Details on the action required to continue the run
data RequiredAction = RequiredAction_Submit_Tool_Outputs
    { submit_tool_outputs :: SubmitToolOutputs
    } deriving stock (Generic, Show)

requiredActionOptions :: Options
requiredActionOptions =
    aesonOptions
        { sumEncoding =
              TaggedObject{ tagFieldName = "type" }

        , tagSingleConstructors = True

        , constructorTagModifier = stripPrefix "RequiredAction_"
        }

instance FromJSON RequiredAction where
    parseJSON = genericParseJSON requiredActionOptions

instance ToJSON RequiredAction where
    toJSON = genericToJSON requiredActionOptions

-- | Details on why the run is incomplete
data IncompleteDetails = IncompleteDetails
    { reason :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Represents an execution run on a thread.
data RunObject = RunObject
    { id :: RunID
    , object :: Text
    , created_at :: POSIXTime
    , thread_id :: ThreadID
    , assistant_id :: AssistantID
    , status :: Status
    , required_action :: Maybe RequiredAction
    , last_error :: Maybe Error
    , expires_at :: Maybe POSIXTime
    , started_at :: Maybe POSIXTime
    , cancelled_at :: Maybe POSIXTime
    , failed_at :: Maybe POSIXTime
    , completed_at :: Maybe POSIXTime
    , incomplete_details :: Maybe IncompleteDetails
    , model :: Model
    , instructions :: Maybe Text
    , tools :: Vector Tool
    , metadata :: Map Text Text
    , usage :: Maybe (Usage (Maybe CompletionTokensDetails) (Maybe PromptTokensDetails))
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , max_prompt_tokens :: Maybe Natural
    , max_completion_tokens :: Maybe Natural
    , truncation_strategy :: Maybe TruncationStrategy
    , tool_choice :: ToolChoice
    , parallel_tool_calls :: Bool
    , response_format :: AutoOr ResponseFormat
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/v1\/threads\/:thread_id\/runs\/:run_id@
data ModifyRun = ModifyRun
    { metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)

instance FromJSON ModifyRun where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ModifyRun where
    toJSON = genericToJSON aesonOptions

-- | Default `ModifyRun`
_ModifyRun :: ModifyRun
_ModifyRun = ModifyRun{ }

-- | A tool for which the output is being submitted
data ToolOutput = ToolOutput
    { tool_call_id :: Maybe Text
    , output :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/v1\/threads\/:thread_id\/runs\/:run_id\/submit_tool_outputs@
data SubmitToolOutputsToRun = SubmitToolOutputsToRun
    { tool_outputs :: Vector ToolOutput
    } deriving stock (Generic, Show)

instance FromJSON SubmitToolOutputsToRun where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON SubmitToolOutputsToRun where
    toJSON = genericToJSON aesonOptions

-- | Default implementation of `SubmitToolOutputsToRun`
_SubmitToolOutputsToRun :: SubmitToolOutputsToRun
_SubmitToolOutputsToRun = SubmitToolOutputsToRun{ }

-- | Servant API
type API =
        "threads"
    :>  Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  (         Capture "thread_id" ThreadID
              :>  "runs"
              :>  QueryParam "include[]" Text
              :>  ReqBody '[JSON] CreateRun
              :>  Post '[JSON] RunObject
        :<|>      "runs"
              :>  ReqBody '[JSON] CreateThreadAndRun
              :>  Post '[JSON] RunObject
        :<|>      Capture "thread_id" ThreadID
              :>  "runs"
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  QueryParam "before" Text
              :>  Get '[JSON] (ListOf RunObject)
        :<|>      Capture "thread_id" ThreadID
              :>  "runs"
              :>  Capture "run_id" RunID
              :>  Get '[JSON] RunObject
        :<|>      Capture "thread_id" ThreadID
              :>  "runs"
              :>  Capture "run_id" RunID
              :>  ReqBody '[JSON] ModifyRun
              :>  Post '[JSON] RunObject
        :<|>      Capture "thread_id" ThreadID
              :>  "runs"
              :>  Capture "run_id" RunID
              :>  "submit_tool_outputs"
              :>  ReqBody '[JSON] SubmitToolOutputsToRun
              :>  Post '[JSON] RunObject
        :<|>      Capture "thread_id" ThreadID
              :>  "runs"
              :>  Capture "run_id" RunID
              :>  "cancel"
              :>  Post '[JSON] RunObject
        )
