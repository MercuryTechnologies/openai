-- | The `Tool` type
module OpenAI.V1.Tool
    ( -- * Types
      Tool(..)
    , RankingOptions(..)
    , FileSearch(..)
    , Function(..)
    , ToolChoice(..)
    , CodeInterpreterContainer(..)
      -- * Helpers
    , codeInterpreter
    , codeInterpreterAuto
    , codeInterpreterWithFiles
    , toolToResponsesValue
    , parseResponsesToolValue
    , toolChoiceToResponsesValue
    , parseResponsesToolChoiceValue
    , flattenToolValue
    , unflattenToolValue
    ) where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import OpenAI.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Set as Set
import qualified Data.Vector as V

-- | The ranking options for the file search
data RankingOptions = RankingOptions
    { ranker :: Maybe Text
    , score_threshold :: Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Overrides for the file search tool
data FileSearch = FileSearch
    { max_num_results :: Maybe Natural
    , ranking_options :: Maybe RankingOptions
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The Function tool
data Function = Function
    { description :: Maybe Text
    , name :: Text
    , parameters :: Maybe Value
    , strict :: Maybe Bool
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A tool enabled on the assistant
data Tool
    = Tool_Code_Interpreter{ container :: Maybe CodeInterpreterContainer }
    | Tool_File_Search{ file_search :: FileSearch }
    | Tool_Function{ function :: Function }
    | Tool_Web_Search
    deriving stock (Generic, Show)

toolOptions :: Options
toolOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True

    , constructorTagModifier = stripPrefix "Tool_"
    }

instance FromJSON Tool where
    parseJSON = genericParseJSON toolOptions

instance ToJSON Tool where
    toJSON = genericToJSON toolOptions

toolToResponsesValue :: Tool -> Value
toolToResponsesValue = flattenToolValue . toJSON

parseResponsesToolValue :: Value -> Parser Tool
parseResponsesToolValue = parseJSON . unflattenToolValue

toolChoiceToResponsesValue :: ToolChoice -> Value
toolChoiceToResponsesValue ToolChoiceNone = "none"
toolChoiceToResponsesValue ToolChoiceAuto = "auto"
toolChoiceToResponsesValue ToolChoiceRequired = "required"
toolChoiceToResponsesValue (ToolChoiceTool tool) = toolToResponsesValue tool

parseResponsesToolChoiceValue :: Value -> Parser ToolChoice
parseResponsesToolChoiceValue (String "none") = pure ToolChoiceNone
parseResponsesToolChoiceValue (String "auto") = pure ToolChoiceAuto
parseResponsesToolChoiceValue (String "required") = pure ToolChoiceRequired
parseResponsesToolChoiceValue other = ToolChoiceTool <$> parseResponsesToolValue other

flattenToolValue :: Value -> Value
flattenToolValue (Aeson.Object o) =
    case KeyMap.lookup (Key.fromText "function") o of
        Just (Aeson.Object fn) ->
            Aeson.Object (KeyMap.delete (Key.fromText "function") o <> fn)
        _ -> Aeson.Object o
flattenToolValue value = value

unflattenToolValue :: Value -> Value
unflattenToolValue (Aeson.Object o) =
    case KeyMap.lookup (Key.fromText "type") o of
        Just (String "function")
            | KeyMap.member (Key.fromText "function") o -> Aeson.Object o
            | otherwise ->
                let (fnFields, rest) = splitFunctionFields o
                    nested = KeyMap.insert (Key.fromText "function") (Aeson.Object fnFields) rest
                in Aeson.Object nested
        _ -> Aeson.Object o
unflattenToolValue value = value

functionFieldKeys :: Set.Set Key.Key
functionFieldKeys = Set.fromList (Key.fromText <$> ["description", "name", "parameters", "strict"])

splitFunctionFields
    :: KeyMap.KeyMap Value
    -> (KeyMap.KeyMap Value, KeyMap.KeyMap Value)
splitFunctionFields = KeyMap.foldrWithKey step (KeyMap.empty, KeyMap.empty)
  where
    step key value (fn, rest)
        | key `Set.member` functionFieldKeys = (KeyMap.insert key value fn, rest)
        | otherwise = (fn, KeyMap.insert key value rest)

-- | Controls which (if any) tool is called by the model
data ToolChoice
    = ToolChoiceNone
    | ToolChoiceAuto
    | ToolChoiceRequired
    | ToolChoiceTool Tool
    deriving stock (Generic, Show)

instance FromJSON ToolChoice where
    parseJSON "none" = pure ToolChoiceNone
    parseJSON "auto" = pure ToolChoiceAuto
    parseJSON "required" = pure ToolChoiceRequired
    parseJSON other = fmap ToolChoiceTool (parseJSON other)

instance ToJSON ToolChoice where
    toJSON ToolChoiceNone = "none"
    toJSON ToolChoiceAuto = "auto"
    toJSON ToolChoiceRequired = "required"
    toJSON (ToolChoiceTool tool) = toJSON tool

-- | Code Interpreter container reference
data CodeInterpreterContainer
    = CodeInterpreterContainer_Auto{ file_ids :: Maybe (Vector Text) }
    | CodeInterpreterContainer_ID{ container_id :: Text }
    deriving stock (Generic, Show)

instance ToJSON CodeInterpreterContainer where
    toJSON (CodeInterpreterContainer_ID container_id) = toJSON container_id
    toJSON (CodeInterpreterContainer_Auto file_ids) =
        Aeson.object $ "type" .= String "auto" :
                      maybe [] (\ids -> ["file_ids" .= ids]) file_ids

instance FromJSON CodeInterpreterContainer where
    parseJSON (String s) = pure $ CodeInterpreterContainer_ID s
    parseJSON (Object o) = do
        t <- o .: "type"
        case (t :: Text) of
            "auto" -> CodeInterpreterContainer_Auto <$> o .:? "file_ids"
            _ -> fail "Unknown code interpreter container object"
    parseJSON _ = fail "Invalid code interpreter container value"

-- | Convenience: Code Interpreter without an explicit container (for Assistants API)
codeInterpreter :: Tool
codeInterpreter =
    Tool_Code_Interpreter { container = Nothing }

-- | Convenience: Code Interpreter with automatic new container and no files
codeInterpreterAuto :: Tool
codeInterpreterAuto =
    Tool_Code_Interpreter
        { container = Just CodeInterpreterContainer_Auto{ file_ids = Nothing }
        }

-- | Convenience: Code Interpreter with automatic new container seeded with files
codeInterpreterWithFiles :: [Text] -> Tool
codeInterpreterWithFiles xs =
    let mFiles = case xs of
            [] -> Nothing
            _  -> Just (V.fromList xs)
    in Tool_Code_Interpreter { container = Just CodeInterpreterContainer_Auto{ file_ids = mFiles } }
