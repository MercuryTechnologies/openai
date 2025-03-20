-- | @\/v1\/assistants@
module OpenAI.V1.Assistants
    ( -- * Main types
      AssistantID(..)
    , CreateAssistant(..)
    , _CreateAssistant
    , ModifyAssistant(..)
    , _ModifyAssistant
    , AssistantObject(..)

      -- * Other types
    , RankingOptions
    , FileSearch(..)
    , Function(..)
    , Tool(..)
    , CodeInterpreterResources(..)
    , FileSearchResources(..)
    , ToolResources(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.AutoOr
import OpenAI.V1.DeletionStatus
import OpenAI.V1.ListOf
import OpenAI.V1.Models (Model)
import OpenAI.V1.Order
import OpenAI.V1.ResponseFormat
import OpenAI.V1.Tool
import OpenAI.V1.ToolResources

-- | AssistantID
newtype AssistantID = AssistantID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/assistants@
data CreateAssistant = CreateAssistant
    { model :: Model
    , name :: Maybe Text
    , description :: Maybe Text
    , instructions :: Maybe Text
    , tools :: Maybe (Vector Tool)
    , tool_resources :: Maybe ToolResources
    , metadata :: Maybe (Map Text Text)
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , response_format :: Maybe (AutoOr ResponseFormat)
    } deriving stock (Generic, Show)

instance FromJSON CreateAssistant where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateAssistant where
    toJSON = genericToJSON aesonOptions

-- | Default `CreateAssistant`
_CreateAssistant :: CreateAssistant
_CreateAssistant = CreateAssistant
    { name = Nothing
    , description = Nothing
    , instructions = Nothing
    , tools = Nothing
    , tool_resources = Nothing
    , metadata = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , response_format = Nothing
    }

-- | Request body for @\/v1\/assistants/:assistant_id@
data ModifyAssistant = ModifyAssistant
    { model :: Model
    , name :: Maybe Text
    , description :: Maybe Text
    , instructions :: Maybe Text
    , tools :: Maybe (Vector Tool)
    , tool_resources :: Maybe ToolResources
    , metadata :: Maybe (Map Text Text)
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , response_format :: Maybe (AutoOr ResponseFormat)
    } deriving stock (Generic, Show)

instance FromJSON ModifyAssistant where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ModifyAssistant where
    toJSON = genericToJSON aesonOptions

-- | Default `ModifyAssistant`
_ModifyAssistant :: ModifyAssistant
_ModifyAssistant = ModifyAssistant
    { name = Nothing
    , description = Nothing
    , instructions = Nothing
    , tools = Nothing
    , tool_resources = Nothing
    , metadata = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , response_format = Nothing
    }

-- | Represents an assistant that can call the model and use tools.
data AssistantObject = AssistantObject
    { id :: AssistantID
    , object :: Text
    , created_at :: POSIXTime
    , name :: Maybe Text
    , description :: Maybe Text
    , model :: Model
    , instructions :: Maybe Text
    , tools :: Maybe (Vector Tool)
    , tool_resources :: Maybe ToolResources
    , metadata :: Map Text Text
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , response_format :: AutoOr ResponseFormat
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  "assistants"
    :>  (         ReqBody '[JSON] CreateAssistant
              :>  Post '[JSON] AssistantObject
        :<|>      QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  QueryParam "before" Text
              :>  Get '[JSON] (ListOf AssistantObject)
        :<|>      Capture "assistant_id" AssistantID
              :>  Get '[JSON] AssistantObject
        :<|>      Capture "assistant_id" AssistantID
              :>  ReqBody '[JSON] ModifyAssistant
              :>  Post '[JSON] AssistantObject
        :<|>      Capture "assistant_id" AssistantID
              :>  Delete '[JSON] DeletionStatus
        )
