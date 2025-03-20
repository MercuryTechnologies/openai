-- | @\/v1\/audio\/speech@
module OpenAI.V1.Audio.Speech
    ( -- * Main types
      CreateSpeech(..)
    , _CreateSpeech
      -- * Other types
    , Voice(..)
    , Format(..)
      -- * Servant
    , ContentType(..)
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Models (Model)

-- | The voice to use when generating the audio
--
-- Previews of the voices are available in the
-- [Text to speech guide](https://platform.openai.com/docs/guides/text-to-speech#voice-options).
data Voice = Alloy | Echo | Fable | Onyx | Nova | Shimmer
    deriving stock (Bounded, Enum, Generic, Show)

instance FromJSON Voice where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Voice where
    toJSON = genericToJSON aesonOptions

-- | The format to audio in
data Format = MP3 | Opus | AAC | FLAC | WAV | PCM
    deriving stock (Bounded, Enum, Generic, Show)

instance FromJSON Format where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Format where
    toJSON = genericToJSON aesonOptions

-- | Request body for @\/v1\/audio\/speech@
data CreateSpeech = CreateSpeech
    { model :: Model
    , input :: Text
    , voice :: Voice
    , response_format :: Maybe Format
    , speed :: Maybe Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateSpeech`
_CreateSpeech :: CreateSpeech
_CreateSpeech = CreateSpeech
    { response_format = Nothing
    , speed = Nothing
    }

-- | Content type
data ContentType = ContentType

instance Accept ContentType where
    contentTypes _ =
            "audio/mpeg"
        :|  [ "audio/flac"
            , "audio/wav"
            , "audio/aac"
            , "audio/opus"
            , "audio/pcm"
            ]

instance MimeUnrender ContentType ByteString where
    mimeUnrender _ bytes = Right bytes

-- | Servant API
type API =
    "speech" :> ReqBody '[JSON] CreateSpeech :> Post '[ContentType] ByteString
