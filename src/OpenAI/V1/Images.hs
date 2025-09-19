-- | @\/v1\/images@
module OpenAI.V1.Images
  ( -- * Servant
    API,
  )
where

import OpenAI.Prelude
import qualified OpenAI.V1.Images.Edits as Edits
import qualified OpenAI.V1.Images.Generations as Generations
import qualified OpenAI.V1.Images.Variations as Variations

-- | Servant API
type API = "images" :> (Generations.API :<|> Edits.API :<|> Variations.API)
