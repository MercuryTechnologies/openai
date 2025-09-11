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
    ) where

import OpenAI.Prelude
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
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
    toJSON CodeInterpreterContainer_ID{ container_id } = toJSON container_id
    toJSON CodeInterpreterContainer_Auto{ file_ids } =
        Aeson.object ( [ "type" .= String "auto" ]
                 <> maybe [] (\ids -> [ ("file_ids", toJSON ids) ]) file_ids
               )

instance FromJSON CodeInterpreterContainer where
    parseJSON v@(String _) = CodeInterpreterContainer_ID <$> parseJSON v
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
