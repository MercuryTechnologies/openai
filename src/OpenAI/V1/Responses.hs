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
    -- Payload types for ResponseStreamEvent
    , ResponseCreatedEventPayload(..)
    , ResponseInProgressEventPayload(..)
    , ResponseCompletedEventPayload(..)
    , ResponseFailedEventPayload(..)
    , ResponseOutputItemAddedEventPayload(..)
    , ResponseOutputItemDoneEventPayload(..)
    , ResponseContentPartAddedEventPayload(..)
    , ResponseContentPartDoneEventPayload(..)
    , ResponseTextDeltaEventPayload(..)
    , ResponseTextDoneEventPayload(..)
    , ResponseOutputTextAnnotationAddedEventPayload(..)
    , ResponseWebSearchCallInProgressEventPayload(..)
    , ResponseWebSearchCallSearchingEventPayload(..)
    , ResponseWebSearchCallCompletedEventPayload(..)
    , ResponseFileSearchCallInProgressEventPayload(..)
    , ResponseFileSearchCallSearchingEventPayload(..)
    , ResponseFileSearchCallCompletedEventPayload(..)
    , ResponseCodeInterpreterCallInProgressEventPayload(..)
    , ResponseCodeInterpreterCallInterpretingEventPayload(..)
    , ResponseCodeInterpreterCallCompletedEventPayload(..)
    , ResponseCodeInterpreterCallCodeDeltaEventPayload(..)
    , ResponseCodeInterpreterCallCodeDoneEventPayload(..)
    , ErrorEventPayload(..)
    , ResponseObject(..)
    , ResponseUsage(..)
    , InputTokensDetails(..)
    , OutputTokensDetails(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude hiding (Input(..))
import Data.Aeson (Object, (.:), (.:?))
-- no TH; inline JSON instances for payloads
import OpenAI.V1.ListOf (ListOf)
import OpenAI.V1.Models (Model)
import OpenAI.V1.Tool (Tool, ToolChoice)
import qualified Data.Text as Text

-- | Input for the Responses API: a list of input items
newtype Input = Input (Vector InputItem)
    deriving stock (Generic, Show)

instance ToJSON Input where
    toJSON (Input xs) = toJSON xs

instance FromJSON Input where
    parseJSON v = Input <$> parseJSON v

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

-- | An input item
data InputItem
    = Item_InputMessage
        { role :: InputRole
        , content :: Vector InputContent
        , status :: Maybe Text
        }
    deriving stock (Generic, Show)

inputItemOptions :: Options
inputItemOptions =
    aesonOptions
        { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
        , tagSingleConstructors = True
        , constructorTagModifier = stripPrefix "Item_Input"
        }

instance FromJSON InputItem where
    parseJSON = genericParseJSON inputItemOptions

instance ToJSON InputItem where
    toJSON = genericToJSON inputItemOptions

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
    parseJSON = genericParseJSON aesonOptions

instance ToJSON OutputMessage where
    toJSON = genericToJSON aesonOptions

-- | A generated output item.
data OutputItem
    = Item_OutputMessage OutputMessage
    | Item_FunctionToolCall FunctionToolCall
    | Item_WebSearchToolCall WebSearchToolCall
    | Item_FunctionToolCallOutput FunctionToolCallOutput
    | Item_FileSearchToolCall FileSearchToolCall
    | Item_CodeInterpreterToolCall CodeInterpreterToolCall
    | Item_Reasoning ReasoningItem
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
            Just other -> fail ("Unknown OutputItem type: " <> Text.unpack other)
            Nothing -> fail "Missing type field for OutputItem"
    parseJSON _ = fail "Expected object for OutputItem"

instance ToJSON OutputItem where
    toJSON (Item_OutputMessage m) = toJSON m
    toJSON (Item_FunctionToolCall m) = toJSON m
    toJSON (Item_WebSearchToolCall m) = toJSON m
    toJSON (Item_FunctionToolCallOutput m) = toJSON m
    toJSON (Item_FileSearchToolCall m) = toJSON m
    toJSON (Item_CodeInterpreterToolCall m) = toJSON m
    toJSON (Item_Reasoning r) = toJSON r

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
    , constructorTagModifier = stripPrefix "Annotation_"
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

-- | Streaming events for /v1/responses
data ResponseStreamEvent
    = ResponseCreatedEvent ResponseCreatedEventPayload
    | ResponseInProgressEvent ResponseInProgressEventPayload
    | ResponseCompletedEvent ResponseCompletedEventPayload
    | ResponseFailedEvent ResponseFailedEventPayload
    | ResponseOutputItemAddedEvent ResponseOutputItemAddedEventPayload
    | ResponseOutputItemDoneEvent ResponseOutputItemDoneEventPayload
    | ResponseContentPartAddedEvent ResponseContentPartAddedEventPayload
    | ResponseContentPartDoneEvent ResponseContentPartDoneEventPayload
    | ResponseTextDeltaEvent ResponseTextDeltaEventPayload
    | ResponseTextDoneEvent ResponseTextDoneEventPayload
    | ResponseOutputTextAnnotationAddedEvent ResponseOutputTextAnnotationAddedEventPayload
    | ResponseWebSearchCallInProgressEvent ResponseWebSearchCallInProgressEventPayload
    | ResponseWebSearchCallSearchingEvent ResponseWebSearchCallSearchingEventPayload
    | ResponseWebSearchCallCompletedEvent ResponseWebSearchCallCompletedEventPayload
    | ResponseFileSearchCallInProgressEvent ResponseFileSearchCallInProgressEventPayload
    | ResponseFileSearchCallSearchingEvent ResponseFileSearchCallSearchingEventPayload
    | ResponseFileSearchCallCompletedEvent ResponseFileSearchCallCompletedEventPayload
    | ResponseCodeInterpreterCallInProgressEvent ResponseCodeInterpreterCallInProgressEventPayload
    | ResponseCodeInterpreterCallInterpretingEvent ResponseCodeInterpreterCallInterpretingEventPayload
    | ResponseCodeInterpreterCallCompletedEvent ResponseCodeInterpreterCallCompletedEventPayload
    | ResponseCodeInterpreterCallCodeDeltaEvent ResponseCodeInterpreterCallCodeDeltaEventPayload
    | ResponseCodeInterpreterCallCodeDoneEvent ResponseCodeInterpreterCallCodeDoneEventPayload
    | ErrorEvent ErrorEventPayload
    deriving stock (Generic, Show)

-- Payload types for selected stream events with derived JSON instances
data ResponseCreatedEventPayload = ResponseCreatedEventPayload
    { response :: ResponseObject
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseInProgressEventPayload = ResponseInProgressEventPayload
    { response :: ResponseObject
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseCompletedEventPayload = ResponseCompletedEventPayload
    { response :: ResponseObject
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseFailedEventPayload = ResponseFailedEventPayload
    { response :: ResponseObject
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseOutputItemAddedEventPayload = ResponseOutputItemAddedEventPayload
    { output_index :: Natural
    , item :: OutputItem
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseOutputItemDoneEventPayload = ResponseOutputItemDoneEventPayload
    { output_index :: Natural
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseContentPartAddedEventPayload = ResponseContentPartAddedEventPayload
    { item_id :: Text
    , output_index :: Natural
    , content_index :: Natural
    , part :: OutputContent
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseContentPartDoneEventPayload = ResponseContentPartDoneEventPayload
    { item_id :: Text
    , output_index :: Natural
    , content_index :: Natural
    , part :: OutputContent
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- Remaining payloads
data ResponseTextDeltaEventPayload = ResponseTextDeltaEventPayload
    { item_id :: Text
    , output_index :: Natural
    , content_index :: Natural
    , delta :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseTextDoneEventPayload = ResponseTextDoneEventPayload
    { item_id :: Text
    , output_index :: Natural
    , content_index :: Natural
    , text :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseOutputTextAnnotationAddedEventPayload = ResponseOutputTextAnnotationAddedEventPayload
    { item_id :: Text
    , output_index :: Natural
    , content_index :: Natural
    , annotation_index :: Natural
    , annotation :: Annotation
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseWebSearchCallInProgressEventPayload = ResponseWebSearchCallInProgressEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseWebSearchCallSearchingEventPayload = ResponseWebSearchCallSearchingEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseWebSearchCallCompletedEventPayload = ResponseWebSearchCallCompletedEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseFileSearchCallInProgressEventPayload = ResponseFileSearchCallInProgressEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseFileSearchCallSearchingEventPayload = ResponseFileSearchCallSearchingEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseFileSearchCallCompletedEventPayload = ResponseFileSearchCallCompletedEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseCodeInterpreterCallInProgressEventPayload = ResponseCodeInterpreterCallInProgressEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseCodeInterpreterCallInterpretingEventPayload = ResponseCodeInterpreterCallInterpretingEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseCodeInterpreterCallCompletedEventPayload = ResponseCodeInterpreterCallCompletedEventPayload
    { output_index :: Natural
    , item_id :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseCodeInterpreterCallCodeDeltaEventPayload = ResponseCodeInterpreterCallCodeDeltaEventPayload
    { output_index :: Natural
    , item_id :: Text
    , delta :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ResponseCodeInterpreterCallCodeDoneEventPayload = ResponseCodeInterpreterCallCodeDoneEventPayload
    { output_index :: Natural
    , item_id :: Text
    , code :: Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

data ErrorEventPayload = ErrorEventPayload
    { code :: Maybe Text
    , message :: Text
    , param :: Maybe Text
    , sequence_number :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

instance FromJSON ResponseStreamEvent where
    parseJSON v@(Object o) = do
        ty <- o .: "type"
        case (ty :: Text) of
            "response.created" -> ResponseCreatedEvent <$> parseJSON v
            "response.in_progress" -> ResponseInProgressEvent <$> parseJSON v
            "response.completed" -> ResponseCompletedEvent <$> parseJSON v
            "response.failed" -> ResponseFailedEvent <$> parseJSON v
            "response.output_item.added" -> ResponseOutputItemAddedEvent <$> parseJSON v
            "response.output_item.done" -> ResponseOutputItemDoneEvent <$> parseJSON v
            "response.content_part.added" -> ResponseContentPartAddedEvent <$> parseJSON v
            "response.content_part.done" -> ResponseContentPartDoneEvent <$> parseJSON v
            "response.output_text.delta" -> ResponseTextDeltaEvent <$> parseJSON v
            "response.output_text.done" -> ResponseTextDoneEvent <$> parseJSON v
            "response.output_text.annotation.added" -> ResponseOutputTextAnnotationAddedEvent <$> parseJSON v
            "response.web_search_call.in_progress" -> ResponseWebSearchCallInProgressEvent <$> parseJSON v
            "response.web_search_call.searching" -> ResponseWebSearchCallSearchingEvent <$> parseJSON v
            "response.web_search_call.completed" -> ResponseWebSearchCallCompletedEvent <$> parseJSON v
            "response.file_search_call.in_progress" -> ResponseFileSearchCallInProgressEvent <$> parseJSON v
            "response.file_search_call.searching" -> ResponseFileSearchCallSearchingEvent <$> parseJSON v
            "response.file_search_call.completed" -> ResponseFileSearchCallCompletedEvent <$> parseJSON v
            "response.code_interpreter_call.in_progress" -> ResponseCodeInterpreterCallInProgressEvent <$> parseJSON v
            "response.code_interpreter_call.interpreting" -> ResponseCodeInterpreterCallInterpretingEvent <$> parseJSON v
            "response.code_interpreter_call.completed" -> ResponseCodeInterpreterCallCompletedEvent <$> parseJSON v
            "response.code_interpreter_call_code.delta" -> ResponseCodeInterpreterCallCodeDeltaEvent <$> parseJSON v
            "response.code_interpreter_call_code.done" -> ResponseCodeInterpreterCallCodeDoneEvent <$> parseJSON v
            "error" -> ErrorEvent <$> parseJSON v
            other -> fail ("Unknown ResponseStreamEvent type: " <> Text.unpack other)
    parseJSON _ = fail "Expected object for ResponseStreamEvent"

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
