-- | @\/v1\/batches@
module OpenAI.Servant.V1.Batches
    ( -- * API
      Request(..)
    , Status(..)
    , Counts(..)
    , Batch(..)
    , API
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.Error
import OpenAI.Servant.V1.ListOf

-- | Request body
data Request = Request
    { input_file_id :: Text
    , endpoint :: Text
    , completion_window :: Text
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON)

-- | The current status of the batch.
data Status
    = Validating
    | Failed
    | In_Progress
    | Finalizing
    | Completed
    | Expired
    | Cancelling
    | Cancelled
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

-- | The request counts for different statuses within the batch.
data Counts = Counts
    { total :: Natural
    , completed :: Natural
    , failed :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

-- | The batch object
data Batch = Batch
    { id :: Text
    , object :: Text
    , batch_endpoint :: Text
    , errors :: Maybe (ListOf Error)
    , batch_input_file_id :: Text
    , batch_completion_window :: Text
    , status :: Status
    , output_file_id :: Maybe Text
    , error_file_id :: Maybe Text
    , created_at :: POSIXTime
    , in_progress_at :: Maybe POSIXTime
    , expires_at :: Maybe POSIXTime
    , finalizing_at :: Maybe POSIXTime
    , completed_at :: Maybe POSIXTime
    , failed_at :: Maybe POSIXTime
    , expired_at :: Maybe POSIXTime
    , cancelling_at :: Maybe POSIXTime
    , cancelled_at :: Maybe POSIXTime
    , request_counts :: Maybe Counts
    , batch_metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)

instance FromJSON Batch where
    parseJSON = genericParseJSON aesonOptions
        { fieldLabelModifier = stripPrefix "batch_" }

-- | API
type API =
        "batches"
    :>  (         ReqBody '[JSON] Request
              :>  Post '[JSON] Batch
        :<|>      Capture "batch_id" Text
              :>  Get '[JSON] Batch
        :<|>      Capture "batch_id" Text
              :>  "cancel"
              :>  Post '[JSON] Batch
        :<|>      QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  Get '[JSON] (ListOf Batch)
        )