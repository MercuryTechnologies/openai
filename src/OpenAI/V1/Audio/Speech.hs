-- | [@\/v1\/audio\/speech@](https://platform.openai.com/docs/api-reference/audio/createSpeech)
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
data Voice = Alloy | Ash | Ballad | Coral | Echo | Fable | Nova | Onyx |  Sage | Shimmer
    deriving stock (Bounded, Enum, Generic, Show)

instance FromJSON Voice where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Voice where
    toJSON = genericToJSON aesonOptions

-- | The format to generate the audio in
data Format = MP3 | Opus | AAC | FLAC | WAV | PCM
    deriving stock (Bounded, Enum, Generic, Show)

instance FromJSON Format where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Format where
    toJSON = genericToJSON aesonOptions

-- | Request body for @\/v1\/audio\/speech@
data CreateSpeech = CreateSpeech
    { model :: Model -- ^ Note that only [TTS models](https://platform.openai.com/docs/models#tts) support speech synthesis.
    , input :: Text -- ^ The text to generate audio for. The maximum length is 4096 characters.
    , voice :: Voice
    , instructions :: Maybe Text -- ^ Instructions for the model to follow when generating the audio.
                                 -- Does not work with @tts-1@ or @tts-1-hd@ models.
    , response_format :: Maybe Format -- ^ Defaults to 'MP3'.
    , speed :: Maybe Double -- ^ Defaults to 1.0 (normal speed). Should be between 0.25 and 4.0.
                            -- Does not work with @gpt-4o-mini-tts@ model.
    } deriving stock (Generic, Show)

instance ToJSON CreateSpeech where
    toJSON = genericToJSON aesonOptions

instance FromJSON CreateSpeech where
    parseJSON = genericParseJSON aesonOptions

-- | Default `CreateSpeech`
_CreateSpeech :: CreateSpeech
_CreateSpeech = CreateSpeech
    { instructions = Nothing
    , response_format = Nothing
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
