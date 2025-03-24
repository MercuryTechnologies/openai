-- | The `ChunkingStrategy` type
module OpenAI.V1.ChunkingStrategy
    ( -- * Main types
      ChunkingStrategy(..)

      -- * Other types
    , Static(..)
    ) where

import OpenAI.Prelude

-- | Static chunking strategy
data Static = Static
    { max_chunk_size_tokens :: Natural
    , chunk_overlap_tokens :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The chunking strategy used to chunk the file(s)
data ChunkingStrategy = ChunkingStrategy_Static{ static :: Static }
    deriving stock (Generic, Show)

chunkingStrategyOptions :: Options
chunkingStrategyOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True

    , constructorTagModifier = stripPrefix "ChunkingStrategy_"
    }

instance FromJSON ChunkingStrategy where
    parseJSON = genericParseJSON chunkingStrategyOptions

instance ToJSON ChunkingStrategy where
    toJSON = genericToJSON chunkingStrategyOptions
