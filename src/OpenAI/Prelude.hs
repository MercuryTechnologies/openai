module OpenAI.Prelude
    ( -- * JSON
      aesonOptions
    , stripPrefix
    , labelModifier
      -- * Multipart Form Data
    , input
    , renderIntegral
    , renderRealFloat
    , getExtension
      -- * Re-exports
    , module Data.Aeson
    , module Data.ByteString.Lazy
    , module Data.List.NonEmpty
    , module Data.Map
    , module Data.String
    , module Data.Text
    , module Data.Time.Clock.POSIX
    , module Data.Vector
    , module Data.Void
    , module Data.Word
    , module GHC.Generics
    , module Numeric.Natural
    , module Servant.API
    , module Servant.Multipart.API
    , module Web.HttpApiData
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Web.HttpApiData (ToHttpApiData(..))

import Data.Aeson
    ( FromJSON(..)
    , Options(..)
    , SumEncoding(..)
    , ToJSON(..)
    , Value(..)
    , genericParseJSON
    , genericToJSON
    )
import Servant.API
    ( Accept(..)
    , Capture
    , Delete
    , Get
    , Header'
    , JSON
    , MimeUnrender(..)
    , OctetStream
    , Optional
    , Post
    , QueryParam
    , ReqBody
    , Required
    , Strict
    , (:<|>)(..)
    , (:>)
    )
import Servant.Multipart.API
    ( FileData(..)
    , Input(..)
    , MultipartData(..)
    , MultipartForm
    , Tmp
    , ToMultipart(..)
    )

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Int
import qualified Data.Text.Lazy.Builder.RealFloat as RealFloat
import qualified System.FilePath as FilePath

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore "_" = ""
dropTrailingUnderscore ""  = ""
dropTrailingUnderscore (c : cs) = c : dropTrailingUnderscore cs

labelModifier :: String -> String
labelModifier = map Char.toLower . dropTrailingUnderscore

stripPrefix :: String -> String -> String
stripPrefix prefix string = labelModifier suffix
  where
    suffix = case List.stripPrefix prefix string of
        Nothing -> string
        Just x  -> x

aesonOptions :: Options
aesonOptions = Aeson.defaultOptions
    { fieldLabelModifier = labelModifier
    , constructorTagModifier = labelModifier
    , omitNothingFields = True
    }

input :: Text -> Text -> [ Input ]
input iName iValue = [ Input{..} ]

renderIntegral :: Integral number => number -> Text
renderIntegral number = Text.Lazy.toStrict (Builder.toLazyText builder)
  where
    builder = Int.decimal number

renderRealFloat :: RealFloat number => number -> Text
renderRealFloat number = Text.Lazy.toStrict (Builder.toLazyText builder)
  where
    builder = RealFloat.formatRealFloat RealFloat.Fixed Nothing number

getExtension :: FilePath -> Text
getExtension file = Text.pack (drop 1 (FilePath.takeExtension file))
