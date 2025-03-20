-- | @\/v1\/threads/:thread_id/messages@
module OpenAI.V1.Threads.Messages
    ( -- * Main types
        MessageID(..)
      , Message(..)
      , ModifyMessage(..)
      , _ModifyMessage
      , MessageObject(..)

      -- * Other types
      , Status(..)
      , IncompleteDetails(..)
      , File(..)
      , Annotation(..)
      , TextObject(..)

      -- * Servant
      , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Assistants (AssistantID)
import OpenAI.V1.DeletionStatus
import OpenAI.V1.Files (FileID)
import OpenAI.V1.ListOf
import OpenAI.V1.Message
import OpenAI.V1.Threads (ThreadID)
import OpenAI.V1.Threads.Runs (RunID)

-- | Message ID
newtype MessageID = MessageID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/threads\/:thread_id\/messages\/:message_id@
data ModifyMessage = ModifyMessage
    { metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)

instance FromJSON ModifyMessage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ModifyMessage where
    toJSON = genericToJSON aesonOptions

-- | Default `ModifyMessage`
_ModifyMessage :: ModifyMessage
_ModifyMessage = ModifyMessage
    { metadata = Nothing
    }

-- | The status of the message
data Status = In_Progress | Incomplete | Completed
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | On an incomplete message, details about why the message is incomplete.
data IncompleteDetails = IncompleteDetails
    { reason :: Text
    } deriving (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | File
data File = File
    { file_id :: FileID
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An annotation
data Annotation
    = File_Citation
        { text :: Text
        , file_citation :: File
        , start_index :: Natural
        , end_index :: Natural
        }
    | File_Path
        { text :: Text
        , file_path :: File
        , start_index :: Natural
        , end_index :: Natural
        }
    deriving stock (Generic, Show)

annotationOptions :: Options
annotationOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON Annotation where
    parseJSON = genericParseJSON annotationOptions

instance ToJSON Annotation where
    toJSON = genericToJSON annotationOptions

-- | The text content that is part of a message.
data TextObject = TextObject
    { value :: Text
    , annotations :: Vector Annotation
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

instance IsString TextObject where
    fromString string =
        TextObject{ value = fromString string, annotations = [] }

-- | Represents a message within a thread.
data MessageObject = MessageObject
    { id :: MessageID
    , object :: Text
    , created_at :: POSIXTime
    , thread_id :: ThreadID
    , status :: Maybe Status
    , incomplete_details :: Maybe IncompleteDetails
    , completed_at :: Maybe POSIXTime
    , incomplete_at :: Maybe POSIXTime
    , role :: Text
    , content :: Vector (Content TextObject)
    , assistant_id :: Maybe AssistantID
    , run_id :: Maybe RunID
    , attachments :: Maybe (Vector Attachment)
    , metadata :: Map Text Text
    } deriving (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  "threads"
    :>  (         Capture "thread_id" ThreadID
              :>  "messages"
              :>  ReqBody '[JSON] Message
              :>  Post '[JSON] MessageObject
        :<|>      Capture "thread_id" ThreadID
              :>  "messages"
              :>  Get '[JSON] (ListOf MessageObject)
        :<|>      Capture "thread_id" ThreadID
              :>  "messages"
              :>  Capture "message_id" MessageID
              :>  Get '[JSON] MessageObject
        :<|>      Capture "thread_id" ThreadID
              :>  "messages"
              :>  Capture "message_id" MessageID
              :>  ReqBody '[JSON] ModifyMessage
              :>  Post '[JSON] MessageObject
        :<|>      Capture "thread_id" ThreadID
              :>  "messages"
              :>  Capture "message_id" MessageID
              :>  Delete '[JSON] DeletionStatus
        )
