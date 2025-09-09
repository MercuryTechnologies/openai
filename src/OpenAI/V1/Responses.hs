-- | /v1/responses
--
-- Streaming is not implemented here; this covers JSON responses only.
module OpenAI.V1.Responses
    ( -- * Main types
      CreateResponse(..)
    , _CreateResponse
    , Input(..)
    , InputItem(..)
    , InputRole(..)
    , InputContent(..)
    , OutputItem(..)
    , OutputMessage(..)
    , OutputContent(..)
    , ResponseObject(..)
    , ResponseUsage(..)
    , InputTokensDetails(..)
    , OutputTokensDetails(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude hiding (Input(..))
import Data.Aeson (Object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import OpenAI.V1.ListOf (ListOf)
import OpenAI.V1.Models (Model)
import OpenAI.V1.Tool (Tool, ToolChoice)

-- | Heterogeneous input for the Responses API
-- A single request input can be a raw text string or a list of input items.
data Input
    = Input_String Text
    | Input_List (Vector InputItem)
    deriving stock (Generic, Show)

instance ToJSON Input where
    toJSON (Input_String t) = String t
    toJSON (Input_List xs) = toJSON xs

instance FromJSON Input where
    parseJSON (String t) = pure (Input_String t)
    parseJSON v = Input_List <$> parseJSON v

-- | Role of an input message
data InputRole = User | System | Developer
    deriving stock (Generic, Show)

instance FromJSON InputRole where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InputRole where
    toJSON = genericToJSON aesonOptions

-- | Content parts for input messages
data InputContent
    = Input_Text { text :: Text }
    | Input_Image { image_url :: Maybe Text, file_id :: Maybe Text, detail :: Maybe Text }
    | Input_File { file_id :: Maybe Text, filename :: Maybe Text, file_url :: Maybe Text, file_data :: Maybe Text }
    | Input_Audio { input_audio :: Object }
    deriving stock (Generic, Show)

inputContentOptions :: Options
inputContentOptions =
    aesonOptions
        { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
        , tagSingleConstructors = True
        -- Keep constructor names like "Input_Text" -> "input_text"
        , constructorTagModifier = labelModifier
        }

instance FromJSON InputContent where
    parseJSON = genericParseJSON inputContentOptions

instance ToJSON InputContent where
    toJSON = genericToJSON inputContentOptions

-- | A single input item. We explicitly support message inputs and pass-through
-- any other unrecognized item shapes without interpretation.
data InputItem
    = Item_InputMessage
        { role :: InputRole
        , content :: Vector InputContent
        , status :: Maybe Text
        }
    | Item_InputUnknown Value
    deriving stock (Show)

instance ToJSON InputItem where
    toJSON Item_InputMessage{ role, content, status } =
        Aeson.object ( [ "type" .= String "message"
                 , "role" .= role
                 , "content" .= content
                 ] <> maybe [] (\s -> [ ("status", toJSON s) ]) status )
    toJSON (Item_InputUnknown v) = v

instance FromJSON InputItem where
    parseJSON v@(Object o) = do
        ty <- o .:? "type"
        case (ty :: Maybe Text) of
            Just "message" -> Item_InputMessage
                <$> o .: "role"
                <*> o .: "content"
                <*> o .:? "status"
            _ -> pure (Item_InputUnknown v)
    parseJSON other = pure (Item_InputUnknown other)

-- | Output content from the model
data OutputContent
    = Output_Text
        { text :: Text
        , annotations :: Vector Value
        , logprobs :: Maybe (Vector Value)
        }
    | Refusal
        { refusal :: Text }
    deriving stock (Generic, Show)

outputContentOptions :: Options
outputContentOptions =
    aesonOptions
        { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
        , tagSingleConstructors = True
        -- Keep constructor names like "Output_Text" -> "output_text"
        , constructorTagModifier = labelModifier
        }

instance FromJSON OutputContent where
    parseJSON = genericParseJSON outputContentOptions

instance ToJSON OutputContent where
    toJSON = genericToJSON outputContentOptions

-- | An output message item
data OutputMessage = OutputMessage
    { id :: Text
    , role :: Text
    , content :: Vector OutputContent
    , status :: Text
    } deriving stock (Generic, Show)

instance FromJSON OutputMessage where
    parseJSON (Object o) = do
        ty <- o .: "type"
        if (ty :: Text) == "message"
            then OutputMessage
                <$> o .: "id"
                <*> o .: "role"
                <*> o .: "content"
                <*> o .: "status"
            else fail "Expected output item of type 'message'"
    parseJSON _ = fail "Expected object for OutputMessage"

instance ToJSON OutputMessage where
    toJSON OutputMessage{ id = oid, role, content, status } =
        Aeson.object
            [ "type" .= String "message"
            , "id" .= oid
            , "role" .= role
            , "content" .= content
            , "status" .= status
            ]

-- | A generated output item. We support message items explicitly and preserve
-- unknown items as raw JSON values for forward compatibility.
data OutputItem
    = Item_OutputMessage OutputMessage
    | Item_OutputUnknown Value
    deriving stock (Show)

instance FromJSON OutputItem where
    parseJSON v@(Object o) = do
        ty <- o .:? "type"
        case (ty :: Maybe Text) of
            Just "message" -> Item_OutputMessage <$> parseJSON v
            _ -> pure (Item_OutputUnknown v)
    parseJSON other = pure (Item_OutputUnknown other)

instance ToJSON OutputItem where
    toJSON (Item_OutputMessage m) = toJSON m
    toJSON (Item_OutputUnknown v) = v

-- | Usage statistics for the response request
data ResponseUsage = ResponseUsage
    { input_tokens :: Natural
    , input_tokens_details :: InputTokensDetails
    , output_tokens :: Natural
    , output_tokens_details :: OutputTokensDetails
    , total_tokens :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data InputTokensDetails = InputTokensDetails
    { cached_tokens :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data OutputTokensDetails = OutputTokensDetails
    { reasoning_tokens :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Response object
data ResponseObject = ResponseObject
    { id :: Text
    , object :: Text
    , created_at :: POSIXTime
    , status :: Text
    , error :: Maybe Value
    , incomplete_details :: Maybe Value
    , instructions :: Maybe Value
    , model :: Model
    , output :: Vector OutputItem
    , parallel_tool_calls :: Bool
    , previous_response_id :: Maybe Text
    , reasoning :: Maybe Value
    , store :: Maybe Bool
    , temperature :: Maybe Double
    , tool_choice :: Maybe ToolChoice
    , tools :: Maybe (Vector Tool)
    , top_p :: Maybe Double
    , truncation :: Maybe Text
    , usage :: Maybe ResponseUsage
    , user :: Maybe Text
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for /v1/responses
data CreateResponse = CreateResponse
    { model :: Model
    , input :: Maybe Input
    , include :: Maybe (Vector Text)
    , parallel_tool_calls :: Maybe Bool
    , store :: Maybe Bool
    , instructions :: Maybe Text
    , stream :: Maybe Bool
    , stream_options :: Maybe Value
    , metadata :: Maybe (Map Text Text)
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , tools :: Maybe (Vector Tool)
    , tool_choice :: Maybe ToolChoice
    } deriving stock (Generic, Show)

instance FromJSON CreateResponse where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateResponse where
    toJSON = genericToJSON aesonOptions

-- | Default CreateResponse
_CreateResponse :: CreateResponse
_CreateResponse = CreateResponse
    { input = Nothing
    , include = Nothing
    , parallel_tool_calls = Nothing
    , store = Nothing
    , instructions = Nothing
    , stream = Nothing
    , stream_options = Nothing
    , metadata = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , tools = Nothing
    , tool_choice = Nothing
    , model = "gpt-4o-mini"
    }

-- | Servant API for /v1/responses
type API =
        "responses"
    :>  (         ReqBody '[JSON] CreateResponse
            :>  Post '[JSON] ResponseObject
        :<|>      Capture "response_id" Text
            :>  Get '[JSON] ResponseObject
        :<|>      Capture "response_id" Text
            :>  "cancel"
            :>  Post '[JSON] ResponseObject
        :<|>      Capture "response_id" Text
            :>  "input_items"
            :>  Get '[JSON] (ListOf InputItem)
        )
