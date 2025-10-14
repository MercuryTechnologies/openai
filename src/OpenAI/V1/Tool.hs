-- | The `Tool` type
module OpenAI.V1.Tool
    ( -- * Types
      Tool(..)
    , RankingOptions(..)
    , FileSearch(..)
    , Function(..)
    , ToolChoice(..)
    , CodeInterpreterContainer(..)
      -- * Constants
    , toolChoiceNoneText
    , toolChoiceAutoText
    , toolChoiceRequiredText
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
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List (partition)
import qualified Data.Vector as V

-- | Tool choice string constants
toolChoiceNoneText, toolChoiceAutoText, toolChoiceRequiredText :: Text
toolChoiceNoneText = "none"
toolChoiceAutoText = "auto"
toolChoiceRequiredText = "required"

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
toolChoiceToResponsesValue ToolChoiceNone = String toolChoiceNoneText
toolChoiceToResponsesValue ToolChoiceAuto = String toolChoiceAutoText
toolChoiceToResponsesValue ToolChoiceRequired = String toolChoiceRequiredText
toolChoiceToResponsesValue (ToolChoiceTool tool) = toolToResponsesValue tool

parseResponsesToolChoiceValue :: Value -> Parser ToolChoice
parseResponsesToolChoiceValue (String s)
    | s == toolChoiceNoneText = pure ToolChoiceNone
    | s == toolChoiceAutoText = pure ToolChoiceAuto
    | s == toolChoiceRequiredText = pure ToolChoiceRequired
parseResponsesToolChoiceValue other = ToolChoiceTool <$> parseResponsesToolValue other

keyFunction, keyType :: Key.Key
keyFunction = Key.fromText "function"
keyType = Key.fromText "type"

functionFieldKeys :: HashSet Key.Key
functionFieldKeys = HashSet.fromList $ Key.fromText <$> ["description", "name", "parameters", "strict"]

isFunctionField :: Key.Key -> Bool
isFunctionField = (`HashSet.member` functionFieldKeys)

partitionFunctionFields
    :: KeyMap.KeyMap Value
    -> (KeyMap.KeyMap Value, KeyMap.KeyMap Value)
partitionFunctionFields obj =
    let (fnPairs, restPairs) = partition (isFunctionField . fst) (KeyMap.toList obj)
    in (KeyMap.fromList fnPairs, KeyMap.fromList restPairs)

flattenToolValue :: Value -> Value
flattenToolValue value@(Aeson.Object o) =
    maybe value flattenFunction (KeyMap.lookup keyFunction o)
  where
    flattenFunction (Aeson.Object fnFields) =
        Aeson.Object (KeyMap.delete keyFunction o <> fnFields)
    flattenFunction _ = value
flattenToolValue value = value

unflattenToolValue :: Value -> Value
unflattenToolValue value@(Aeson.Object o)
    | KeyMap.lookup keyType o == Just (String "function")
    , not (KeyMap.member keyFunction o) =
        let (fnFields, rest) = partitionFunctionFields o
        in Aeson.Object (KeyMap.insert keyFunction (Aeson.Object fnFields) rest)
    | otherwise = value
unflattenToolValue value = value

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
