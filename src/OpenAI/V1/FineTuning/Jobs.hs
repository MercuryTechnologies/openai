-- | @\/v1\/fine_tuning/jobs@
module OpenAI.V1.FineTuning.Jobs
    ( -- * Main types
      FineTuningJobID(..)
    , CreateFineTuningJob(..)
    , _CreateFineTuningJob
    , JobObject(..)
    , EventObject(..)
    , CheckpointObject(..)

      -- * Other types
    , AutoOr(..)
    , Hyperparameters(..)
    , WAndB(..)
    , Integration(..)
    , Status(..)
    , Level(..)
    , Metrics(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.AutoOr
import OpenAI.V1.Error
import OpenAI.V1.Files (FileID)
import OpenAI.V1.Models (Model)
import OpenAI.V1.ListOf

-- | Fine tuning job ID
newtype FineTuningJobID = FineTuningJobID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | The hyperparameters used for the fine-tuning job
data Hyperparameters = Hyperparameters
    { batch_size :: Maybe (AutoOr Natural)
    , learning_rate_multiplier :: Maybe (AutoOr Double)
    , n_epochs :: Maybe (AutoOr Natural)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The settings for your integration with Weights and
data WAndB = WAndB
    { project :: Text
    , name :: Maybe Text
    , entity :: Maybe Text
    , tags :: Maybe (Vector Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An integration to enable for your fine-tuning job
data Integration = Integration_WAndB{ wandb :: WAndB }
    deriving stock (Generic, Show)

integrationOptions :: Options
integrationOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True

    , constructorTagModifier = stripPrefix "Integration_"
    }

instance FromJSON Integration where
    parseJSON = genericParseJSON integrationOptions

instance ToJSON Integration where
    toJSON = genericToJSON integrationOptions

-- | Request body for @\/v1\/fine_tuning\/jobs@
data CreateFineTuningJob = CreateFineTuningJob
    { model :: Model
    , training_file :: FileID
    , hyperparameters :: Maybe Hyperparameters
    , suffix :: Maybe Text
    , validation_file :: Maybe FileID
    , integrations :: Maybe (Vector Integration)
    , seed :: Maybe Integer
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateFineTuningJob`
_CreateFineTuningJob :: CreateFineTuningJob
_CreateFineTuningJob = CreateFineTuningJob
    { hyperparameters = Nothing
    , suffix = Nothing
    , validation_file = Nothing
    , integrations = Nothing
    , seed = Nothing
    }

-- | The current status of the fine-tuning job
data Status
    = Validating_Files
    | Queued
    | Running
    | Succeeded
    | Failed
    | Cancelled
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | The fine_tuning.job object represents a fine-tuning job that has been
-- created through the API.
data JobObject = JobObject
    { id :: FineTuningJobID
    , created_at :: POSIXTime
    , error :: Maybe Error
    , fine_tuned_model :: Maybe Model
    , finished_at :: Maybe POSIXTime
    , hyperparameters :: Hyperparameters
    , model :: Model
    , object :: Text
    , organization_id :: Text
    , result_files :: Vector FileID
    , status :: Status
    , trained_tokens :: Maybe Natural
    , training_file :: FileID
    , validation_file :: Maybe FileID
    , integrations :: Maybe (Vector Integration)
    , seed :: Integer
    , estimated_finish :: Maybe POSIXTime
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Log level
data Level = Info | Warn | Error
    deriving stock (Generic, Show)

instance FromJSON Level where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Level where
    toJSON = genericToJSON aesonOptions

-- | Fine-tuning job event object
data EventObject = EventObject
    { id :: Text
    , created_at :: POSIXTime
    , level :: Level
    , message :: Text
    , object :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Metrics at the step number during the fine-tuning job.
data Metrics = Metrics
    { step :: Double
    , train_loss :: Double
    , train_mean_token_accuracy :: Double
    , valid_loss :: Double
    , valid_mean_token_accuracy :: Double
    , full_valid_loss :: Double
    , full_valid_mean_token_accuracy :: Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The @fine_tuning.job.checkpoint@ object represents a model checkpoint for
-- a fine-tuning job that is ready to use
data CheckpointObject = CheckpointObject
    { id :: Text
    , created_at :: POSIXTime
    , fine_tuned_model_checkpoint :: Text
    , step_number :: Natural
    , metrics :: Metrics
    , fine_tuning_job_id :: FineTuningJobID
    , object :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "fine_tuning"
    :>  "jobs"
    :>  (         ReqBody '[JSON] CreateFineTuningJob
              :>  Post '[JSON] JobObject
        :<|>      QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  Get '[JSON] (ListOf JobObject)
        :<|>      Capture "fine_tuning_job_id" FineTuningJobID
              :>  "events"
              :>  QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  Get '[JSON] (ListOf EventObject)
        :<|>      Capture "fine_tuning_job_id" FineTuningJobID
              :>  "checkpoints"
              :>  QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  Get '[JSON] (ListOf CheckpointObject)
        :<|>      Capture "fine_tuning_job_id" FineTuningJobID
              :>  Get '[JSON] JobObject
        :<|>      Capture "fine_tuning_job_id" FineTuningJobID
              :>  "cancel"
              :>  Post '[JSON] JobObject
        )
