-- | The `Message` type
module OpenAI.V1.Message
    ( -- * Main types
      Message(..)

      -- * Other types
    , ImageFile(..)
    , ImageURL(..)
    , Content(..)
    , Attachment(..)
    ) where

import OpenAI.Prelude
import OpenAI.V1.AutoOr
import OpenAI.V1.Files (FileID)
import OpenAI.V1.Tool

-- | References an image File in the content of a message
data ImageFile = ImageFile{ file_id :: FileID, detail :: Maybe (AutoOr Text) }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | References an image URL in the content of a message
data ImageURL = ImageURL
    { image_url :: Text
    , detail :: Maybe (AutoOr Text)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Message content
data Content text
    = Image_File{ image_file :: ImageFile }
    | Image_URL{ image_url :: ImageURL }
    | Text{ text :: text }
    deriving stock (Generic, Show)

contentOptions :: Options
contentOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True
    }

instance FromJSON text => FromJSON (Content text) where
    parseJSON = genericParseJSON contentOptions

instance ToJSON text => ToJSON (Content text) where
    toJSON = genericToJSON contentOptions

instance IsString text => IsString (Content text) where
    fromString string = Text{ text = fromString string }

-- | A file attached to the message, and the tools it should be added to
data Attachment = Attachment{ file_id :: FileID, tools :: Maybe (Vector Tool) }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | A message
data Message
    = User
        { content :: Vector (Content Text)
        , attachments :: Maybe (Vector Attachment)
        , metadata :: Maybe (Map Text Text)
        }
    | Assistant
        { content :: Vector (Content Text)
        , attachments :: Maybe (Vector Attachment)
        , metadata :: Maybe (Map Text Text)
        }
    deriving stock (Generic, Show)

messageOptions :: Options
messageOptions =
    aesonOptions
        { sumEncoding =
            TaggedObject{ tagFieldName = "role", contentsFieldName = "" }
        , tagSingleConstructors = True
        }

instance FromJSON Message where
    parseJSON = genericParseJSON  messageOptions

instance ToJSON Message where
    toJSON = genericToJSON  messageOptions
