-- | The `ToolCall` type
module OpenAI.V1.ToolCall
  ( -- * Types
    ToolCall (..),
    Function (..),
  )
where

import OpenAI.Prelude

-- | A called function
data Function = Function {name :: Text, arguments :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Tools called by the model
data ToolCall = ToolCall_Function
  { id :: Text
  , function :: Function
  }
  deriving stock (Generic, Show)

toolCallOptions :: Options
toolCallOptions =
  aesonOptions
    { sumEncoding =
        TaggedObject {tagFieldName = "type", contentsFieldName = ""}
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefix "ToolCall_"
    }

instance ToJSON ToolCall where
  toJSON = genericToJSON toolCallOptions

instance FromJSON ToolCall where
  parseJSON = genericParseJSON toolCallOptions
