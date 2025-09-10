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
    , FunctionToolCall(..)
    , FunctionToolCallOutput(..)
    , WebSearchToolCall(..)
    , FileSearchToolCall(..)
    , FileSearchResult(..)
    , CodeInterpreterToolCall(..)
    , CodeInterpreterOutput(..)
    , WebSearchAction(..)
    , WebSearchSource(..)
    , Annotation(..)
    , ReasoningItem(..)
    , SummaryPart(..)
    , ReasoningText(..)
    , ResponseStreamEvent(..)
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
    | Item_FunctionToolCall FunctionToolCall
    | Item_WebSearchToolCall WebSearchToolCall
    | Item_FunctionToolCallOutput FunctionToolCallOutput
    | Item_FileSearchToolCall FileSearchToolCall
    | Item_CodeInterpreterToolCall CodeInterpreterToolCall
    | Item_Reasoning ReasoningItem
    | Item_OutputUnknown Value
    deriving stock (Show)

instance FromJSON OutputItem where
    parseJSON v@(Object o) = do
        ty <- o .:? "type"
        case (ty :: Maybe Text) of
            Just "message" -> Item_OutputMessage <$> parseJSON v
            Just "function_call" -> Item_FunctionToolCall <$> parseJSON v
            Just "web_search_call" -> Item_WebSearchToolCall <$> parseJSON v
            Just "file_search_call" -> Item_FileSearchToolCall <$> parseJSON v
            Just "code_interpreter_call" -> Item_CodeInterpreterToolCall <$> parseJSON v
            Just "function_call_output" -> Item_FunctionToolCallOutput <$> parseJSON v
            Just "reasoning" -> Item_Reasoning <$> parseJSON v
            _ -> pure (Item_OutputUnknown v)
    parseJSON other = pure (Item_OutputUnknown other)

instance ToJSON OutputItem where
    toJSON (Item_OutputMessage m) = toJSON m
    toJSON (Item_FunctionToolCall m) = toJSON m
    toJSON (Item_WebSearchToolCall m) = toJSON m
    toJSON (Item_FunctionToolCallOutput m) = toJSON m
    toJSON (Item_FileSearchToolCall m) = toJSON m
    toJSON (Item_CodeInterpreterToolCall m) = toJSON m
    toJSON (Item_Reasoning r) = toJSON r
    toJSON (Item_OutputUnknown v) = v

-- | Function tool call output item
data FunctionToolCall = FunctionToolCall
    { id :: Maybe Text
    , call_id :: Text
    , name :: Text
    , arguments :: Text
    , status :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Function tool call output item
data FunctionToolCallOutput = FunctionToolCallOutput
    { id :: Maybe Text
    , call_id :: Text
    , output :: Text
    , status :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Web search tool call output item (action is left generic for now)
data WebSearchToolCall = WebSearchToolCall
    { id :: Text
    , status :: Text
    , action :: Maybe WebSearchAction
    } deriving stock (Generic, Show)

instance FromJSON WebSearchToolCall where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON WebSearchToolCall where
    toJSON = genericToJSON aesonOptions

-- | File search result entry
data FileSearchResult = FileSearchResult
    { file_id :: Text
    , text :: Text
    , filename :: Text
    , score :: Maybe Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | File search tool call output item
data FileSearchToolCall = FileSearchToolCall
    { id :: Text
    , status :: Text
    , queries :: Vector Text
    , results :: Maybe (Vector FileSearchResult)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Code interpreter tool call outputs
data CodeInterpreterOutput
    = CodeInterpreterOutput_Logs{ logs :: Text }
    | CodeInterpreterOutput_Image{ url :: Text }
    deriving stock (Generic, Show)

codeInterpreterOutputOptions :: Options
codeInterpreterOutputOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefix "CodeInterpreterOutput_"
    }

instance FromJSON CodeInterpreterOutput where
    parseJSON = genericParseJSON codeInterpreterOutputOptions

instance ToJSON CodeInterpreterOutput where
    toJSON = genericToJSON codeInterpreterOutputOptions

-- | Code interpreter tool call output item
data CodeInterpreterToolCall = CodeInterpreterToolCall
    { id :: Text
    , status :: Text
    , container_id :: Maybe Text
    , code :: Maybe Text
    , outputs :: Maybe (Vector CodeInterpreterOutput)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Web search action sources
data WebSearchSource = WebSearchSource_URL{ url :: Text }
    deriving stock (Generic, Show)

webSearchSourceOptions :: Options
webSearchSourceOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefix "WebSearchSource_"
    }

instance FromJSON WebSearchSource where
    parseJSON = genericParseJSON webSearchSourceOptions

instance ToJSON WebSearchSource where
    toJSON = genericToJSON webSearchSourceOptions

-- | Web search action
data WebSearchAction
    = WebSearchAction_Search
        { query :: Maybe Text
        , sources :: Maybe (Vector WebSearchSource)
        }
    | WebSearchAction_Open_Page
        { url :: Maybe Text }
    | WebSearchAction_Find
        { url :: Maybe Text
        , pattern :: Maybe Text
        }
    deriving stock (Generic, Show)

webSearchActionOptions :: Options
webSearchActionOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefix "WebSearchAction_"
    }

instance FromJSON WebSearchAction where
    parseJSON = genericParseJSON webSearchActionOptions

instance ToJSON WebSearchAction where
    toJSON = genericToJSON webSearchActionOptions

-- | Output text annotation
data Annotation
    = Annotation_File_Citation
        { file_id :: Text
        , index :: Natural
        , filename :: Text
        }
    | Annotation_Url_Citation
        { url :: Text
        , start_index :: Natural
        , end_index :: Natural
        , title :: Text
        }
    | Annotation_Container_File_Citation
        { container_id :: Text
        , file_id :: Text
        , start_index :: Natural
        , end_index :: Natural
        , filename :: Text
        }
    | Annotation_File_Path
        { file_id :: Text
        , index :: Natural
        }
    deriving stock (Generic, Show)

annotationOptions :: Options
annotationOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier =
        ( \c -> case c of
            "Annotation_File_Citation" -> "file_citation"
            "Annotation_Url_Citation" -> "url_citation"
            "Annotation_Container_File_Citation" -> "container_file_citation"
            "Annotation_File_Path" -> "file_path"
            other -> labelModifier other
        )
    }

instance FromJSON Annotation where
    parseJSON = genericParseJSON annotationOptions

instance ToJSON Annotation where
    toJSON = genericToJSON annotationOptions

-- | Reasoning summary part
data SummaryPart = Summary_Text{ text :: Text }
    deriving stock (Generic, Show)

summaryPartOptions :: Options
summaryPartOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    }

instance FromJSON SummaryPart where
    parseJSON = genericParseJSON summaryPartOptions

instance ToJSON SummaryPart where
    toJSON = genericToJSON summaryPartOptions

-- | Reasoning text part
data ReasoningText = Reasoning_Text{ text :: Text }
    deriving stock (Generic, Show)

reasoningTextOptions :: Options
reasoningTextOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    }

instance FromJSON ReasoningText where
    parseJSON = genericParseJSON reasoningTextOptions

instance ToJSON ReasoningText where
    toJSON = genericToJSON reasoningTextOptions

-- | Reasoning item produced by reasoning models
data ReasoningItem = ReasoningItem
    { id :: Text
    , encrypted_content :: Maybe Text
    , summary :: Maybe (Vector SummaryPart)
    , content :: Maybe (Vector ReasoningText)
    , status :: Maybe Text
    } deriving stock (Generic, Show)

instance FromJSON ReasoningItem where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ReasoningItem where
    toJSON = genericToJSON aesonOptions

-- | Streaming events for /v1/responses (core subset with fallback)
data ResponseStreamEvent
    = ResponseCreatedEvent
        { response :: ResponseObject
        , sequence_number :: Natural
        }
    | ResponseInProgressEvent
        { response :: ResponseObject
        , sequence_number :: Natural
        }
    | ResponseCompletedEvent
        { response :: ResponseObject
        , sequence_number :: Natural
        }
    | ResponseFailedEvent
        { response :: ResponseObject
        , sequence_number :: Natural
        }
    | ResponseOutputItemAddedEvent
        { output_index :: Natural
        , item :: OutputItem
        , sequence_number :: Natural
        }
    | ResponseOutputItemDoneEvent
        { output_index :: Natural
        , sequence_number :: Natural
        }
    | ResponseContentPartAddedEvent
        { item_id :: Text
        , output_index :: Natural
        , content_index :: Natural
        , part :: OutputContent
        , sequence_number :: Natural
        }
    | ResponseContentPartDoneEvent
        { item_id :: Text
        , output_index :: Natural
        , content_index :: Natural
        , part :: OutputContent
        , sequence_number :: Natural
        }
    | ResponseTextDeltaEvent
        { item_id :: Text
        , output_index :: Natural
        , content_index :: Natural
        , delta :: Text
        , sequence_number :: Natural
        }
    | ResponseTextDoneEvent
        { item_id :: Text
        , output_index :: Natural
        , content_index :: Natural
        , text :: Text
        , sequence_number :: Natural
        }
    | ResponseOutputTextAnnotationAddedEvent
        { item_id :: Text
        , output_index :: Natural
        , content_index :: Natural
        , annotation_index :: Natural
        , annotation :: Annotation
        , sequence_number :: Natural
        }
    | ResponseWebSearchCallInProgressEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseWebSearchCallSearchingEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseWebSearchCallCompletedEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseFileSearchCallInProgressEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseFileSearchCallSearchingEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseFileSearchCallCompletedEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseCodeInterpreterCallInProgressEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseCodeInterpreterCallInterpretingEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseCodeInterpreterCallCompletedEvent
        { output_index :: Natural
        , item_id :: Text
        , sequence_number :: Natural
        }
    | ResponseCodeInterpreterCallCodeDeltaEvent
        { output_index :: Natural
        , item_id :: Text
        , delta :: Text
        , sequence_number :: Natural
        }
    | ResponseCodeInterpreterCallCodeDoneEvent
        { output_index :: Natural
        , item_id :: Text
        , code :: Text
        , sequence_number :: Natural
        }
    | ErrorEvent
        { error_code :: Maybe Text
        , message :: Text
        , param :: Maybe Text
        , sequence_number :: Natural
        }
    | UnknownEvent Value
    deriving stock (Show)

instance FromJSON ResponseStreamEvent where
    parseJSON v@(Object o) = do
        ty <- o .: "type"
        case (ty :: Text) of
            "response.created" -> ResponseCreatedEvent
                <$> o .: "response"
                <*> o .: "sequence_number"
            "response.in_progress" -> ResponseInProgressEvent
                <$> o .: "response"
                <*> o .: "sequence_number"
            "response.completed" -> ResponseCompletedEvent
                <$> o .: "response"
                <*> o .: "sequence_number"
            "response.failed" -> ResponseFailedEvent
                <$> o .: "response"
                <*> o .: "sequence_number"
            "response.output_item.added" -> ResponseOutputItemAddedEvent
                <$> o .: "output_index"
                <*> o .: "item"
                <*> o .: "sequence_number"
            "response.output_item.done" -> ResponseOutputItemDoneEvent
                <$> o .: "output_index"
                <*> o .: "sequence_number"
            "response.content_part.added" -> ResponseContentPartAddedEvent
                <$> o .: "item_id"
                <*> o .: "output_index"
                <*> o .: "content_index"
                <*> o .: "part"
                <*> o .: "sequence_number"
            "response.content_part.done" -> ResponseContentPartDoneEvent
                <$> o .: "item_id"
                <*> o .: "output_index"
                <*> o .: "content_index"
                <*> o .: "part"
                <*> o .: "sequence_number"
            "response.output_text.delta" -> ResponseTextDeltaEvent
                <$> o .: "item_id"
                <*> o .: "output_index"
                <*> o .: "content_index"
                <*> o .: "delta"
                <*> o .: "sequence_number"
            "response.output_text.done" -> ResponseTextDoneEvent
                <$> o .: "item_id"
                <*> o .: "output_index"
                <*> o .: "content_index"
                <*> o .: "text"
                <*> o .: "sequence_number"
            "response.output_text.annotation.added" -> ResponseOutputTextAnnotationAddedEvent
                <$> o .: "item_id"
                <*> o .: "output_index"
                <*> o .: "content_index"
                <*> o .: "annotation_index"
                <*> o .: "annotation"
                <*> o .: "sequence_number"
            "response.web_search_call.in_progress" -> ResponseWebSearchCallInProgressEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.web_search_call.searching" -> ResponseWebSearchCallSearchingEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.web_search_call.completed" -> ResponseWebSearchCallCompletedEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.file_search_call.in_progress" -> ResponseFileSearchCallInProgressEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.file_search_call.searching" -> ResponseFileSearchCallSearchingEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.file_search_call.completed" -> ResponseFileSearchCallCompletedEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.code_interpreter_call.in_progress" -> ResponseCodeInterpreterCallInProgressEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.code_interpreter_call.interpreting" -> ResponseCodeInterpreterCallInterpretingEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.code_interpreter_call.completed" -> ResponseCodeInterpreterCallCompletedEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "sequence_number"
            "response.code_interpreter_call_code.delta" -> ResponseCodeInterpreterCallCodeDeltaEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "delta"
                <*> o .: "sequence_number"
            "response.code_interpreter_call_code.done" -> ResponseCodeInterpreterCallCodeDoneEvent
                <$> o .: "output_index"
                <*> o .: "item_id"
                <*> o .: "code"
                <*> o .: "sequence_number"
            "error" -> ErrorEvent
                <$> o .:? "code"
                <*> o .: "message"
                <*> o .:? "param"
                <*> o .: "sequence_number"
            _ -> pure (UnknownEvent v)
    parseJSON other = pure (UnknownEvent other)

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
