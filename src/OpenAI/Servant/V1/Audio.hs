-- | @\/v1\/audio@
module OpenAI.Servant.V1.Audio
    ( -- * API
      API
    ) where

import OpenAI.Servant.Prelude
import qualified OpenAI.Servant.V1.Audio.Speech as Speech
import qualified OpenAI.Servant.V1.Audio.Transcriptions as Transcriptions
import qualified OpenAI.Servant.V1.Audio.Translations as Translations

-- | API
type API =
    "audio" :> (Speech.API :<|> Transcriptions.API :<|> Translations.API)
