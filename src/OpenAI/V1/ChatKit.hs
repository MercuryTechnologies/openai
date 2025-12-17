-- | @\/v1\/chatkit@
module OpenAI.V1.ChatKit
    ( -- * Main types
      SessionID(..)
    , ThreadID(..)
    , CreateChatKitSession(..)
    , _CreateChatKitSession
    , ChatSessionObject(..)
    , CancelChatSession(..)
    , ThreadObject(..)
    , ThreadItem(..)
      -- * Other types
    , Workflow(..)
    , ChatKitConfiguration(..)
    , ExpiresAfter(..)
    , RateLimits(..)
    , Tracing(..)
    , AutomaticThreadTitling(..)
    , FileUpload(..)
    , History(..)
    , Scope(..)
    , Attachment(..)
    , UserContent(..)
    , InferenceOptions(..)
    , ToolChoice(..)
    , AssistantContent(..)
    , Annotation(..)
    , FileSource(..)
    , URLSource(..)
    , Task(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.DeletionStatus (DeletionStatus(..))
import OpenAI.V1.ListOf (ListOf(..))
import OpenAI.V1.Order (Order(..))

-- | Session ID
newtype SessionID = SessionID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Thread ID
newtype ThreadID = ThreadID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/chatkit\/sessions@
data CreateChatKitSession = CreateChatKitSession
    { user :: Text
    , workflow :: Workflow
    , chatkit_configuration :: Maybe ChatKitConfiguration
    , expires_after :: Maybe ExpiresAfter
    , rate_limits :: Maybe RateLimits
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateChatKitSession`
_CreateChatKitSession :: CreateChatKitSession
_CreateChatKitSession = CreateChatKitSession
    { chatkit_configuration = Nothing
    , expires_after = Nothing
    , rate_limits = Nothing
    }

-- | Workflow that powers the session
data Workflow = Workflow
    { id :: Text
    , state_variables :: Maybe (Map Text Value)
    , tracing :: Maybe Tracing
    , version :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Tracing overrides for the workflow invocation
data Tracing = Tracing
    { enabled :: Maybe Bool
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Overrides for ChatKit runtime configuration features
data ChatKitConfiguration = ChatKitConfiguration
    { automatic_thread_titling :: Maybe AutomaticThreadTitling
    , file_upload :: Maybe FileUpload
    , history :: Maybe History
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration for automatic thread titling
data AutomaticThreadTitling = AutomaticThreadTitling
    { enabled :: Maybe Bool
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration for file upload enablement and limits
data FileUpload = FileUpload
    { enabled :: Maybe Bool
    , max_file_size :: Maybe Natural
    , max_files :: Maybe Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration for chat history retention
data History = History
    { enabled :: Maybe Bool
    , recent_threads :: Maybe Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Override for session expiration timing
data ExpiresAfter = Created_At
    { seconds :: NominalDiffTime
    } deriving stock (Generic, Show)

expiresAfterOptions :: Options
expiresAfterOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject
                { tagFieldName = "expires_after", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON ExpiresAfter where
    parseJSON = genericParseJSON expiresAfterOptions

instance ToJSON ExpiresAfter where
    toJSON = genericToJSON expiresAfterOptions

-- | Override for per-minute request limits
data RateLimits = RateLimits
    { max_requests_per_1_minute :: Maybe Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A ChatKit session
data ChatSessionObject = ChatSessionObject
    { chatkit_configuration :: ChatKitConfiguration
    , client_secret :: Text
    , expires_at :: POSIXTime
    , id :: SessionID
    , max_requests_per_1_minute :: Natural
    , object :: Text
    , rate_limits :: RateLimits
    , status :: Text
    , user :: Text
    , workflow :: Workflow
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Response body for @\/v1\/chatkit\/sessions\/{session_id}\/cancel@
data CancelChatSession = CancelChatSession
    { id :: SessionID
    , workflow :: Workflow
    , scope :: Scope
    , max_requests_per_1_minute :: Natural
    , ttl_seconds :: NominalDiffTime
    , status :: Text
    , cancelled_at :: POSIXTime
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Scope
data Scope = Scope
    { customer_id :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A ChatKit thread
data ThreadObject = ThreadObject
    { created_at :: POSIXTime
    , id :: ThreadID
    , object :: Text
    , status :: Status
    , title :: Maybe Text
    , user :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Current status for the thread
data Status = Active
    deriving stock (Generic, Show)

statusOptions :: Options
statusOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON Status where
    parseJSON = genericParseJSON statusOptions

instance ToJSON Status where
    toJSON = genericToJSON statusOptions

-- | A thread item
data ThreadItem
    = User_Message
        { attachments :: Vector Attachment
        , content :: Vector UserContent
        , created_at :: POSIXTime
        , id :: Text
        , inference_options :: InferenceOptions
        , object :: Text
        , thread_id :: Text
        }
    | Assistant_Message
        { assistant_content :: Vector AssistantContent
        , created_at :: POSIXTime
        , id :: Text
        , object :: Text
        , thread_id :: Text
        }
    | Widget_Message
        { created_at :: POSIXTime
        , id :: Text
        , object :: Text
        , thread_id :: Text
        , widget :: Text
        }
    | Client_Tool_Call
        { arguments :: Text
        , call_id :: Text
        , created_at :: POSIXTime
        , id :: Text
        , name :: Text
        , object :: Text
        , output :: Text
        , status :: Text
        , thread_id :: Text
        }
    | Task_Item
        { created_at :: POSIXTime
        , heading :: Text
        , id :: Text
        , object :: Text
        , summary :: Text
        , task_type
        , thread_id :: Text
        }
    | Task_Group
        { created_at :: POSIXTime
        , id :: Text
        , object :: Text
        , tasks :: Vector Task
        , thread_id :: Text
        } deriving stock (Generic, Show)

threadItemOptions :: Options
threadItemOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True

        , fieldLabelModifier = stripPrefix "assistant_"
        }

instance FromJSON ThreadItem where
    parseJSON = genericParseJSON threadItemOptions

instance ToJSON ThreadItem where
    toJSON = genericToJSON threadItemOptions

-- | Attachment associated with the user message
data Attachment = Attachment
    { id :: Text
    , mime_type :: Text
    , name :: Text
    , preview_url :: Text
    } deriving stock (Generic, Show)

instance FromJSON Attachment where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Attachment where
    toJSON = genericToJSON aesonOptions

-- | Content element supplied by the user
data UserContent
    = Input_Text{ text :: Text }
    | Quoted_Text{ text :: Text }
    deriving stock (Generic, Show)

userContentOptions :: Options
userContentOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON UserContent where
    parseJSON = genericParseJSON userContentOptions

instance ToJSON UserContent where
    toJSON = genericToJSON userContentOptions

-- | Inference overrides applies to the message
data InferenceOptions = InferenceOptions
    { model :: Text
    , tool_choice :: ToolChoice
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Preferred tool to invoke
data ToolChoice = ToolChoice
    { id :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Assistant response segment
data AssistantContent = Output_Text
    { annotations :: Vector Annotation
    , text :: Text
    } deriving stock (Generic, Show)

assistantContentOptions :: Options
assistantContentOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True
        }

instance FromJSON AssistantContent where
    parseJSON = genericParseJSON assistantContentOptions

instance ToJSON AssistantContent where
    toJSON = genericToJSON assistantContentOptions

-- | Annotation
data Annotation
    = File
        { source :: FileSource
        }
    | Url
        { url_source :: URLSource
        }
    deriving stock (Generic, Show)

annotationOptions :: Options
annotationOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , tagSingleConstructors = True

        , fieldLabelModifier = stripPrefix "url_"
        }

instance FromJSON Annotation where
    parseJSON = genericParseJSON annotationOptions

instance ToJSON Annotation where
    toJSON = genericToJSON annotationOptions

-- | File attachment
data FileSource = FileSource_File
    { filename :: Text
    } deriving stock (Generic, Show)

fileSourceOptions :: Options
fileSourceOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , constructorTagModifier = stripPrefix "FileSource_"

        , tagSingleConstructors = True
        }

instance FromJSON FileSource where
    parseJSON = genericParseJSON fileSourceOptions

instance ToJSON FileSource where
    toJSON = genericToJSON fileSourceOptions

-- | URL
data URLSource = URLSource_Url
    { url :: Text
    } deriving stock (Generic, Show)

urlSourceOptions :: Options
urlSourceOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

        , constructorTagModifier = stripPrefix "URLSource_"

        , tagSingleConstructors = True
        }

instance FromJSON URLSource where
    parseJSON = genericParseJSON urlSourceOptions

instance ToJSON URLSource where
    toJSON = genericToJSON urlSourceOptions

-- | Task
data Task = Task
    { heading :: Text
    , summary :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
          "chatkit"
    :>    (     (   "sessions"
                :>  ReqBody '[JSON] CreateChatKitSession
                :>  Post '[JSON] ChatSessionObject
                )
          :<|>  (   "sessions"
                :>  Capture "session_id" SessionID
                :>  "cancel"
                :>  Post '[JSON] CancelChatSession
                )
          :<|>  (   "threads"
                :>  QueryParam "after" Text
                :>  QueryParam "before" Text
                :>  QueryParam "limit" Natural
                :>  QueryParam "order" Order
                :>  QueryParam "user" Text
                :>  Get '[JSON] (ListOf ThreadObject)
                )
          :<|>  (   "threads"
                :>  Capture "thread_id" ThreadID
                :>  Get '[JSON] ThreadObject
                )
          :<|>  (   "threads"
                :>  Capture "thread_id" ThreadID
                :>  Delete '[JSON] DeletionStatus
                )
          :<|>  (   "threads"
                :>  Capture "thread_id" ThreadID
                :>  "items"
                :>  QueryParam "after" Text
                :>  QueryParam "before" Text
                :>  QueryParam "limit" Natural
                :>  QueryParam "order" Order
                :>  Get '[JSON] (ListOf ThreadItem)
                )
          )
