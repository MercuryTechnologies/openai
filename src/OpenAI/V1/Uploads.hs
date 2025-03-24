-- | @\/v1\/uploads@
{-# LANGUAGE InstanceSigs #-}
module OpenAI.V1.Uploads
    ( -- * Main types
      UploadID(..)
    , CreateUpload(..)
    , _CreateUpload
    , AddUploadPart(..)
    , _AddUploadPart
    , CompleteUpload(..)
    , _CompleteUpload
    , UploadObject(..)
    , PartObject(..)
      -- * Other types
    , Status(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Files (FileObject, Purpose)

import qualified Data.Text as Text

-- | Upload ID
newtype UploadID = UploadID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/uploads@
data CreateUpload = CreateUpload
    { filename :: Text
    , purpose :: Purpose
    , bytes :: Natural
    , mime_type :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateUpload`
_CreateUpload :: CreateUpload
_CreateUpload = CreateUpload{ }

-- | Request body for @\/v1\/uploads\/:upload_id\/parts@
data AddUploadPart = AddUploadPart{ data_ :: FilePath }
    deriving stock (Generic, Show)

instance FromJSON AddUploadPart where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON AddUploadPart where
    toJSON = genericToJSON aesonOptions

-- | Default `AddUploadPart`
_AddUploadPart :: AddUploadPart
_AddUploadPart = AddUploadPart{ }

instance ToMultipart Tmp AddUploadPart where
    toMultipart AddUploadPart{..} = MultipartData{..}
      where
        inputs = mempty

        files = [ FileData{..} ]
          where
            fdInputName = "data"
            fdFileName = Text.pack data_
            fdFileCType = "application/octet-stream"
            fdPayload = data_

-- | Request body for @\/v1\/uploads\/:upload_id\/complete@
data CompleteUpload = CompleteUpload
    { part_ids :: Vector Text
    , md5 :: Maybe Text
    } deriving stock (Generic, Show)

-- | Default `CompleteUpload`
_CompleteUpload :: CompleteUpload
_CompleteUpload = CompleteUpload
    { md5 = Nothing
    }

-- OpenAI says that the `md5` field is optional, but what they really mean is
-- that it can be set to the empty string.  We still model it as `Maybe Text`,
-- but convert `Nothing` to an empty string at encoding time.
instance ToJSON CompleteUpload where
    toJSON completeUpload = genericToJSON aesonOptions (fix completeUpload)
      where
        fix CompleteUpload{ md5 = Nothing, .. } =
            CompleteUpload{ md5 = Just "", .. }
        fix x = x

instance FromJSON CompleteUpload where
    parseJSON = fmap (fmap fix) (genericParseJSON aesonOptions)
      where
        fix CompleteUpload{ md5 = Just "", .. } =
            CompleteUpload{ md5 = Nothing, .. }
        fix x = x

-- | The status of the Upload
data Status
    = Pending
    | Completed
    | Cancelled
    | Expired
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | The Upload object can accept byte chunks in the form of Parts.
data UploadObject file = UploadObject
    { id :: UploadID
    , created_at :: POSIXTime
    , filename :: Text
    , bytes :: Natural
    , purpose :: Purpose
    , status :: Status
    , expires_at :: POSIXTime
    , object :: Text
    , file :: file
    } deriving stock (Generic, Show)

instance FromJSON (UploadObject (Maybe Void))
instance FromJSON (UploadObject FileObject)
instance ToJSON (UploadObject (Maybe Void))
instance ToJSON (UploadObject FileObject)

-- | The upload part represents a chunk of bytes we can add to an upload
-- object
data PartObject = PartObject
    { id :: Text
    , created_at :: POSIXTime
    , upload_id :: UploadID
    , object :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API
    = "uploads"
    :>  (         ReqBody '[JSON] CreateUpload
              :>  Post '[JSON] (UploadObject (Maybe Void))
        :<|>      Capture "upload_id" UploadID
              :>  "parts"
              :>  MultipartForm Tmp AddUploadPart
              :>  Post '[JSON] PartObject
        :<|>      Capture "upload_id" UploadID
              :>  "complete"
              :>  ReqBody '[JSON] CompleteUpload
              :>  Post '[JSON] (UploadObject FileObject)
        :<|>      Capture "upload_id" UploadID
              :>  "cancel"
              :>  Post '[JSON] (UploadObject (Maybe Void))
        )
