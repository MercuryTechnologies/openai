-- | @\/v1\/files@
module OpenAI.V1.Files
    ( -- * Main types
      FileID(..)
    , UploadFile(..)
    , _UploadFile
    , FileObject(..)
    -- * Other types
    , Order(..)
    , Purpose(..)
    , DeletionStatus(..)
    -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.DeletionStatus
import OpenAI.V1.ListOf
import OpenAI.V1.Order

import qualified Data.Text as Text

-- | File ID
newtype FileID = FileID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | UploadFile body
data UploadFile = UploadFile
    { file :: FilePath
    , purpose :: Purpose
    } deriving stock (Generic, Show)

-- | Default `UploadFile`
_UploadFile :: UploadFile
_UploadFile = UploadFile{ }

instance ToMultipart Tmp UploadFile where
    toMultipart UploadFile{..} = MultipartData{..}
      where
        inputs = input "purpose" (toUrlPiece purpose)

        files = [ FileData{..} ]
          where
            fdInputName = "file"
            fdFileName = Text.pack file
            fdFileCType = "application/json"
            fdPayload = file

-- | The File object represents a document that has been uploaded to OpenAI
data FileObject = FileObject
    { id :: FileID
    , bytes :: Natural
    , created_at :: POSIXTime
    , filename :: Text
    , object :: Text
    , purpose :: Purpose
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The intended purpose of the uploaded file.
data Purpose
    = Assistants
    | Assistants_Output
    | Batch
    | Batch_Output
    | Fine_Tune
    | Fine_Tune_Results
    | Vision
    deriving stock (Generic, Show)

purposeOptions :: Options
purposeOptions = aesonOptions
    { constructorTagModifier = fix . labelModifier }
  where
    fix "fine_tune" = "fine-tune"
    fix "fine_tune_results" = "fine-tune-results"
    fix string = string

instance FromJSON Purpose where
    parseJSON = genericParseJSON purposeOptions

instance ToJSON Purpose where
    toJSON = genericToJSON purposeOptions

instance ToHttpApiData Purpose where
    toUrlPiece Assistants = "assistants"
    toUrlPiece Assistants_Output = "assistants"
    toUrlPiece Batch = "batch"
    toUrlPiece Batch_Output = "batch_output"
    toUrlPiece Fine_Tune = "fine-tune"
    toUrlPiece Fine_Tune_Results = "fine-tune-results"
    toUrlPiece Vision = "vision"

-- | Servant API
type API =
        "files"
    :>  (         MultipartForm Tmp UploadFile
              :>  Post '[JSON] FileObject
        :<|>      QueryParam "purpose" Purpose
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  Get '[JSON] (ListOf FileObject)
        :<|>      Capture "file_id" FileID
              :>  Get '[JSON] FileObject
        :<|>      Capture "file_id" FileID
              :>  Delete '[JSON] DeletionStatus
        :<|>      Capture "file_id" FileID
              :>  "content"
              :>  Get '[OctetStream] ByteString
        )
