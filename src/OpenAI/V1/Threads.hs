-- | @\/v1\/threads@
module OpenAI.V1.Threads
    ( -- Main types
      ThreadID(..)
    , Thread(..)
    , _Thread
    , ModifyThread(..)
    , _ModifyThread
    , Message(..)
    , Content(..)
    , ThreadObject(..)

      -- * Other types
    , ImageURL(..)
    , ImageFile(..)
    , Attachment(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.DeletionStatus
import OpenAI.V1.Message
import OpenAI.V1.ToolResources

-- | Thread ID
newtype ThreadID = ThreadID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/threads@
data Thread = Thread
    { messages :: Maybe (Vector Message)
    , tool_resources :: Maybe ToolResources
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `Thread`
_Thread :: Thread
_Thread = Thread
    { messages = Nothing
    , tool_resources = Nothing
    , metadata = Nothing
    }

-- | Request body for @\/v1\/threads\/:thread_id@
data ModifyThread = ModifyThread
    { tool_resources :: Maybe ToolResources
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)

instance FromJSON ModifyThread where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ModifyThread where
    toJSON = genericToJSON aesonOptions

-- | Default `ModifyThread`
_ModifyThread :: ModifyThread
_ModifyThread = ModifyThread
    { tool_resources = Nothing
    , metadata = Nothing
    }

-- | Represents a thread that contains messages
data ThreadObject = ThreadObject
    { id :: ThreadID
    , object :: Text
    , created_at :: POSIXTime
    , tool_resources :: Maybe ToolResources
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "threads"
    :>  Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  (         ReqBody '[JSON] Thread
              :>  Post '[JSON] ThreadObject
        :<|>      Capture "thread_id" ThreadID
              :>  Get '[JSON] ThreadObject
        :<|>      Capture "thread_id" ThreadID
              :>  ReqBody '[JSON] ModifyThread
              :>  Post '[JSON] ThreadObject
        :<|>      Capture "thread_id" ThreadID
              :>  Delete '[JSON] DeletionStatus
        )
