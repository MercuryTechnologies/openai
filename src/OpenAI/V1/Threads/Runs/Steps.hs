-- | @\/v1\/threads\/:thread_id\/runs\/:run_id\/steps@
module OpenAI.V1.Threads.Runs.Steps
    ( -- * Main types
      StepID(..)
    , RunStepObject(..)

      -- * Other types
    , Status(..)
    , Image(..)
    , Output(..)
    , CodeInterpreter(..)
    , RankingOptions(..)
    , Content(..)
    , Result(..)
    , FileSearch(..)
    , Function(..)
    , ToolCall(..)
    , StepDetails(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Assistants (AssistantID)
import OpenAI.V1.Error
import OpenAI.V1.Files (FileID)
import OpenAI.V1.ListOf
import OpenAI.V1.Threads.Messages (MessageID)
import OpenAI.V1.Order
import OpenAI.V1.Threads (ThreadID)
import OpenAI.V1.Threads.Runs (RunID)
import OpenAI.V1.Usage

-- | Step ID
newtype StepID = StepID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | The status of the run step
data Status = In_Progress | Cancelled | Failed | Completed | Expired
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | Code Interpreter image output
data Image = Image{ file_id :: FileID }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | An output from the Code Interpreter tool call
data Output = Output_Logs{ logs :: Text } | Output_Image{ image :: Image }
    deriving stock (Generic, Show)

outputOptions :: Options
outputOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True

        , constructorTagModifier = stripPrefix "Output_"
        }

instance FromJSON Output where
    parseJSON = genericParseJSON outputOptions

instance ToJSON Output where
    toJSON = genericToJSON outputOptions

-- | A Code Interpreter tool call
data CodeInterpreter = CodeInterpreter
    { input :: Text
    , outputs :: Vector Output
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The ranking options for the file search.
data RankingOptions = RankingOptions
    { ranker :: Text
    , score_threshold :: Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The content of the result that was found
data Content = Content
    { type_ :: Text
    , text :: Text
    } deriving stock (Generic, Show)

instance FromJSON Content where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Content where
    toJSON = genericToJSON aesonOptions

-- | Result of the file search
data Result = Result
    { file_id :: FileID
    , file_name :: Text
    , score :: Double
    , content :: Vector Content
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A File Search tool call
data FileSearch = FileSearch
    { ranking_options :: RankingOptions
    , results :: Vector Result
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The definition of the function that was called
data Function = Function
    { name :: Text
    , arguments :: Text
    , output :: Maybe Text
    } deriving (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A tool call the run step was involved in
data ToolCall
    = ToolCall_Code_Interpreter { id :: Text, code_interpreter :: CodeInterpreter }
    | ToolCall_File_Search { id :: Text, file_search :: Map Text FileSearch }
    | ToolCall_Function { id :: Text, function :: Function }
    deriving stock (Generic, Show)

toolCallOptions :: Options
toolCallOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True

        , constructorTagModifier = stripPrefix "ToolCall_"
        }

instance FromJSON ToolCall where
    parseJSON = genericParseJSON toolCallOptions

instance ToJSON ToolCall where
    toJSON = genericToJSON toolCallOptions

-- | The details of the run step
data StepDetails
    = Message_Creation{ message_id :: MessageID }
    | Tool_Calls{ tool_calls :: Vector ToolCall }
    deriving stock (Generic, Show)

stepDetailsOptions :: Options
stepDetailsOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON StepDetails where
    parseJSON = genericParseJSON stepDetailsOptions

instance ToJSON StepDetails where
    toJSON = genericToJSON stepDetailsOptions

-- | Represents a step in execution of a run.
data RunStepObject = RunStepObject
    { id :: StepID
    , object :: Text
    , created_at :: POSIXTime
    , assistant_id :: AssistantID
    , thread_id :: ThreadID
    , run_id :: RunID
    , status :: Status
    , step_details :: StepDetails
    , last_error :: Maybe Error
    , expired_at :: Maybe POSIXTime
    , cancelled_at :: Maybe POSIXTime
    , failed_at :: Maybe POSIXTime
    , completed_at :: Maybe POSIXTime
    , metadata :: Map Text Text
    , usage :: Maybe (Usage CompletionTokensDetails PromptTokensDetails)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "threads"
    :>  Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  (     Capture "thread_id" ThreadID
              :>  "runs"
              :>  Capture "run_id" RunID
              :>  "steps"
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  QueryParam "before" Text
              :>  QueryParam "include[]" Text
              :>  Get '[JSON] (ListOf RunStepObject)
        :<|>      Capture "thread_id" ThreadID
              :>  "runs"
              :>  Capture "run_id" RunID
              :>  "steps"
              :>  Capture "step_id" StepID
              :>  QueryParam "include[]" Text
              :>  Get '[JSON] RunStepObject
        )
