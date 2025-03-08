-- | The `Usage` type
module OpenAI.V1.Usage
    ( -- * Main types
      Usage(..)

      -- * Other types
    , CompletionTokensDetails(..)
    , PromptTokensDetails(..)
    ) where

import OpenAI.Prelude

-- | Breakdown of tokens used in a completion
data CompletionTokensDetails = CompletionTokensDetails
    { accepted_prediction_tokens :: Maybe Natural
    , audio_tokens :: Maybe Natural
    , reasoning_tokens :: Maybe Natural
    , rejected_prediction_tokens :: Maybe Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

-- | Breakdown of tokens used in the prompt
data PromptTokensDetails = PromptTokensDetails
    { audio_tokens :: Maybe Natural
    , cached_tokens :: Maybe Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

-- | Usage statistics for the completion request
data Usage completionTokensDetails promptTokensDetails = Usage
    { completion_tokens :: Natural
    , prompt_tokens :: Natural
    , total_tokens :: Natural
    , completion_tokens_details :: completionTokensDetails
    , prompt_tokens_details :: promptTokensDetails
    } deriving stock (Generic, Show)

instance FromJSON (Usage CompletionTokensDetails PromptTokensDetails)
instance FromJSON (Usage (Maybe CompletionTokensDetails) (Maybe PromptTokensDetails))
