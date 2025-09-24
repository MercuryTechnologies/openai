-- | @\/v1\/audio@
module OpenAI.V1.Audio
    ( -- * Servant
      API
    ) where

import OpenAI.Prelude

import qualified OpenAI.V1.Audio.Speech as Speech
import qualified OpenAI.V1.Audio.Transcriptions as Transcriptions
import qualified OpenAI.V1.Audio.Translations as Translations

-- | Servant API
type API =
    "audio" :> (Speech.API :<|> Transcriptions.API :<|> Translations.API)
