-- | @\/v1\/batches@
module OpenAI.V1.Batches
    ( -- * Main types
      BatchID(..)
    , CreateBatch(..)
    , _CreateBatch
    , BatchObject(..)
      -- * Other types
    , Status(..)
    , Counts(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Error
import OpenAI.V1.Files (FileID)
import OpenAI.V1.ListOf

-- | Batch ID
newtype BatchID = BatchID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/batches@
data CreateBatch = CreateBatch
    { input_file_id :: FileID
    , endpoint :: Text
    , completion_window :: Text
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateBatch`
_CreateBatch :: CreateBatch
_CreateBatch = CreateBatch
    { metadata = Nothing
    }

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

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | The request counts for different statuses within the batch.
data Counts = Counts
    { total :: Natural
    , completed :: Natural
    , failed :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The batch object
data BatchObject = BatchObject
    { id :: BatchID
    , object :: Text
    , endpoint :: Text
    , errors :: Maybe (ListOf Error)
    , input_file_id :: FileID
    , completion_window :: Text
    , status :: Status
    , output_file_id :: Maybe FileID
    , error_file_id :: Maybe FileID
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
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)

instance FromJSON BatchObject where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON BatchObject where
    toJSON = genericToJSON aesonOptions

-- | Servant API
type API =
        "batches"
    :>  (         ReqBody '[JSON] CreateBatch
              :>  Post '[JSON] BatchObject
        :<|>      Capture "batch_id" BatchID
              :>  Get '[JSON] BatchObject
        :<|>      Capture "batch_id" BatchID
              :>  "cancel"
              :>  Post '[JSON] BatchObject
        :<|>      QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  Get '[JSON] (ListOf BatchObject)
        )
