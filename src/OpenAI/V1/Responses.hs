-- | \/v1/responses
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
      -- * Constants
    , statusIncomplete
    , statusCompleted
      -- * Servant
    , API
    ) where

import Data.Aeson (Object)
import qualified Data.Aeson as Aeson
import OpenAI.Prelude hiding (Input(..))
-- no TH; inline JSON instances for payloads
import OpenAI.V1.ListOf (ListOf)
import OpenAI.V1.Models (Model)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import OpenAI.V1.Tool
    ( Tool
    , ToolChoice
    , toolChoiceAutoText
    , toolChoiceNoneText
    , toolChoiceRequiredText
    , toolChoiceToResponsesValue
    , toolToResponsesValue
    , unflattenToolValue
    )

-- | Status constants for function call outputs
statusIncomplete, statusCompleted :: Text
statusIncomplete = "incomplete"
statusCompleted = "completed"

-- | Input for the Responses API: a list of input items
newtype Input = Input (Vector InputItem)
    deriving stock (Generic, Show)

instance ToJSON Input where
    toJSON (Input xs) = toJSON xs

instance FromJSON Input where
    parseJSON v = Input <$> parseJSON v

-- | Role of an input message
data InputRole = User | System | Developer
    deriving stock (Eq, Generic, Show)

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
    deriving stock (Eq, Generic, Show)

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
    = Item_Input_Message
        { role :: InputRole
        , content :: Vector InputContent
        , status :: Maybe Text
        }
    | Item_Input_Function_Call
        { id :: Maybe Text
        , call_id :: Text
        , name :: Text
        , arguments :: Text
        , status :: Maybe Text
        }
    | Item_Input_Function_Call_Output
        { id :: Maybe Text
        , call_id :: Text
        , output :: Text
        , status :: Maybe Text
        }
    | Item_Input_Reasoning
        { reasoning_id :: Text
        , reasoning_encrypted_content :: Maybe Text
        , reasoning_summary :: Maybe (Vector SummaryPart)
        , reasoning_content :: Maybe (Vector ReasoningText)
        , reasoning_status :: Maybe Text
        }
    | Item_Input_Item_Reference
        { id :: Maybe Text
        }
    deriving stock (Eq, Generic, Show)

inputItemOptions :: Options
inputItemOptions =
    aesonOptions
        { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
        , tagSingleConstructors = True
        , constructorTagModifier = stripPrefix "Item_Input_"
        , fieldLabelModifier = stripPrefix "reasoning_"
        }

instance FromJSON InputItem where
    parseJSON = genericParseJSON inputItemOptions

instance ToJSON InputItem where
    toJSON = genericToJSON inputItemOptions

keyTools :: Key.Key
keyTools = Key.fromText "tools"

keyToolChoice :: Key.Key
keyToolChoice = Key.fromText "tool_choice"

flattenResponseToolFields
    :: Maybe (Vector Tool)
    -> Maybe ToolChoice
    -> KeyMap.KeyMap Value
    -> KeyMap.KeyMap Value
flattenResponseToolFields mTools mChoice o =
    let oWithTools = case mTools of
            Nothing -> o
            Just ts -> KeyMap.insert keyTools (Aeson.Array (Vector.map toolToResponsesValue ts)) o
        oWithChoice = case mChoice of
            Nothing -> oWithTools
            Just choice -> KeyMap.insert keyToolChoice (toolChoiceToResponsesValue choice) oWithTools
    in oWithChoice

unflattenResponseToolFields :: KeyMap.KeyMap Value -> KeyMap.KeyMap Value
unflattenResponseToolFields = adjustChoice . adjustTools
  where
    adjustTools = adjustKey keyTools (mapArrayValues unflattenToolValue)
    adjustChoice = adjustKey keyToolChoice unflattenChoice

    unflattenChoice (String s)
        | s `elem` ([toolChoiceNoneText, toolChoiceAutoText, toolChoiceRequiredText] :: [Text]) = String s
    unflattenChoice other = unflattenToolValue other

mapArrayValues :: (Value -> Value) -> Value -> Value
mapArrayValues f (Aeson.Array arr) = Aeson.Array (Vector.map f arr)
mapArrayValues _ other = other

adjustKey
    :: Key.Key
    -> (Value -> Value)
    -> KeyMap.KeyMap Value
    -> KeyMap.KeyMap Value
adjustKey key f obj =
    case KeyMap.lookup key obj of
        Nothing -> obj
        Just v -> KeyMap.insert key (f v) obj

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
    = Item_OutputMessage
        { message_id :: Text
        , message_role :: Text
        , message_content :: Vector OutputContent
        , message_status :: Text
        }
    | Item_FunctionToolCall
        { function_id :: Maybe Text
        , function_call_id :: Text
        , function_name :: Text
        , function_arguments :: Text
        , function_status :: Maybe Text
        }
    | Item_WebSearchToolCall
        { web_search_id :: Text
        , web_search_status :: Text
        , web_search_action :: Maybe WebSearchAction
        }
    | Item_FunctionToolCallOutput
        { function_output_id :: Maybe Text
        , function_output_call_id :: Text
        , function_output_output :: Text
        , function_output_status :: Maybe Text
        }
    | Item_FileSearchToolCall
        { file_search_id :: Text
        , file_search_status :: Text
        , file_search_queries :: Vector Text
        , file_search_results :: Maybe (Vector FileSearchResult)
        }
    | Item_CodeInterpreterToolCall
        { code_interpreter_id :: Text
        , code_interpreter_status :: Text
        , code_interpreter_container_id :: Maybe Text
        , code_interpreter_code :: Maybe Text
        , code_interpreter_outputs :: Maybe (Vector CodeInterpreterOutput)
        }
    | Item_Reasoning
        { reasoning_id :: Text
        , reasoning_encrypted_content :: Maybe Text
        , reasoning_summary :: Maybe (Vector SummaryPart)
        , reasoning_content :: Maybe (Vector ReasoningText)
        , reasoning_status :: Maybe Text
        }
    deriving stock (Generic, Show)

outputItemOptions :: Options
outputItemOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , fieldLabelModifier = \s -> case s of
        "message_id" -> "id"
        "message_role" -> "role"
        "message_content" -> "content"
        "message_status" -> "status"
        "function_id" -> "id"
        "function_call_id" -> "call_id"
        "function_name" -> "name"
        "function_arguments" -> "arguments"
        "function_status" -> "status"
        "web_search_id" -> "id"
        "web_search_status" -> "status"
        "web_search_action" -> "action"
        "function_output_id" -> "id"
        "function_output_call_id" -> "call_id"
        "function_output_output" -> "output"
        "function_output_status" -> "status"
        "file_search_id" -> "id"
        "file_search_status" -> "status"
        "file_search_queries" -> "queries"
        "file_search_results" -> "results"
        "code_interpreter_id" -> "id"
        "code_interpreter_status" -> "status"
        "code_interpreter_container_id" -> "container_id"
        "code_interpreter_code" -> "code"
        "code_interpreter_outputs" -> "outputs"
        "reasoning_id" -> "id"
        "reasoning_encrypted_content" -> "encrypted_content"
        "reasoning_summary" -> "summary"
        "reasoning_content" -> "content"
        "reasoning_status" -> "status"
        other -> other
    , constructorTagModifier = \s -> case s of
        "Item_OutputMessage" -> "message"
        "Item_FunctionToolCall" -> "function_call"
        "Item_WebSearchToolCall" -> "web_search_call"
        "Item_FunctionToolCallOutput" -> "function_call_output"
        "Item_FileSearchToolCall" -> "file_search_call"
        "Item_CodeInterpreterToolCall" -> "code_interpreter_call"
        "Item_Reasoning" -> "reasoning"
        _ -> Prelude.error "Unknown OutputItem constructor"
    }

instance FromJSON OutputItem where
    parseJSON = genericParseJSON outputItemOptions

instance ToJSON OutputItem where
    toJSON = genericToJSON outputItemOptions

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
    deriving stock (Eq, Generic, Show)

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
    deriving stock (Eq, Generic, Show)

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
    } deriving stock (Eq, Generic, Show)

instance FromJSON ReasoningItem where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ReasoningItem where
    toJSON = genericToJSON aesonOptions

-- | Streaming events for \/v1/responses
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
        , error_message :: Text
        , error_param :: Maybe Text
        , error_sequence_number :: Natural
        }
    deriving stock (Generic, Show)

responseStreamEventOptions :: Options
responseStreamEventOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , fieldLabelModifier = \s -> case s of
        -- Strip "error_" prefix from ErrorEvent fields
        "error_code" -> "code"
        "error_message" -> "message"
        "error_param" -> "param"
        "error_sequence_number" -> "sequence_number"
        -- Keep all other fields as-is
        other -> other
    , constructorTagModifier = \s -> case s of
        "ResponseCreatedEvent" -> "response.created"
        "ResponseInProgressEvent" -> "response.in_progress"
        "ResponseCompletedEvent" -> "response.completed"
        "ResponseFailedEvent" -> "response.failed"
        "ResponseOutputItemAddedEvent" -> "response.output_item.added"
        "ResponseOutputItemDoneEvent" -> "response.output_item.done"
        "ResponseContentPartAddedEvent" -> "response.content_part.added"
        "ResponseContentPartDoneEvent" -> "response.content_part.done"
        "ResponseTextDeltaEvent" -> "response.output_text.delta"
        "ResponseTextDoneEvent" -> "response.output_text.done"
        "ResponseOutputTextAnnotationAddedEvent" -> "response.output_text.annotation.added"
        "ResponseWebSearchCallInProgressEvent" -> "response.web_search_call.in_progress"
        "ResponseWebSearchCallSearchingEvent" -> "response.web_search_call.searching"
        "ResponseWebSearchCallCompletedEvent" -> "response.web_search_call.completed"
        "ResponseFileSearchCallInProgressEvent" -> "response.file_search_call.in_progress"
        "ResponseFileSearchCallSearchingEvent" -> "response.file_search_call.searching"
        "ResponseFileSearchCallCompletedEvent" -> "response.file_search_call.completed"
        "ResponseCodeInterpreterCallInProgressEvent" -> "response.code_interpreter_call.in_progress"
        "ResponseCodeInterpreterCallInterpretingEvent" -> "response.code_interpreter_call.interpreting"
        "ResponseCodeInterpreterCallCompletedEvent" -> "response.code_interpreter_call.completed"
        "ResponseCodeInterpreterCallCodeDeltaEvent" -> "response.code_interpreter_call_code.delta"
        "ResponseCodeInterpreterCallCodeDoneEvent" -> "response.code_interpreter_call_code.done"
        "ErrorEvent" -> "error"
        _ -> Prelude.error "Unknown ResponseStreamEvent constructor"
    }

instance FromJSON ResponseStreamEvent where
    parseJSON = genericParseJSON responseStreamEventOptions

instance ToJSON ResponseStreamEvent where
    toJSON = genericToJSON responseStreamEventOptions


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

instance FromJSON ResponseObject where
    parseJSON (Aeson.Object o) =
        genericParseJSON aesonOptions (Aeson.Object (unflattenResponseToolFields o))
    parseJSON other = genericParseJSON aesonOptions other

instance ToJSON ResponseObject where
    toJSON response@ResponseObject{ tools = mTools, tool_choice = mChoice } =
        case genericToJSON aesonOptions response of
            Aeson.Object o ->
                Aeson.Object
                    (flattenResponseToolFields mTools mChoice o)
            other -> other

-- | Request body for \/v1/responses
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
    parseJSON (Aeson.Object o) =
        genericParseJSON aesonOptions (Aeson.Object (unflattenResponseToolFields o))
    parseJSON other = genericParseJSON aesonOptions other

instance ToJSON CreateResponse where
    toJSON request@CreateResponse{ tools = mTools, tool_choice = mChoice } =
        case genericToJSON aesonOptions request of
            Aeson.Object o ->
                Aeson.Object
                    (flattenResponseToolFields mTools mChoice o)
            other -> other

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

-- | Servant API for \/v1/responses
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
