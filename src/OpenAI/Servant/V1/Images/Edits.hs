-- | @\/v1\/images\/edits@
module OpenAI.Servant.V1.Images.Edits
    ( -- * API
      ResponseFormat(..)
    , CreateImageEdit(..)
    , API
    ) where

import OpenAI.Servant.Prelude
import OpenAI.Servant.V1.Images.Image
import OpenAI.Servant.V1.Images.ResponseFormat
import OpenAI.Servant.V1.ListOf

import qualified Data.Text as Text

-- | Request body for @\/v1\/images\/edits@
data CreateImageEdit = CreateImageEdit
    { image :: FilePath
    , prompt :: Text
    , mask :: Maybe FilePath
    , model :: Maybe Text
    , n :: Maybe Natural
    , size :: Maybe Text
    , response_format :: Maybe ResponseFormat
    , user :: Maybe Text
    } deriving stock (Generic, Show)

instance ToMultipart Tmp CreateImageEdit where
    toMultipart CreateImageEdit{..} = MultipartData{..}
      where
        inputs =
                input "prompt" prompt
            <>  foldMap (input "model") model
            <>  foldMap (input "n" . renderIntegral) n
            <>  foldMap (input "size") size
            <>  foldMap (input "response_format" . toUrlPiece) response_format
            <>  foldMap (input "user") user

        files = file0 : files1
          where
            file0 = FileData{..}
              where
                fdInputName = "image"
                fdFileName = Text.pack image
                fdFileCType = "image/" <> getExtension image
                fdPayload = image

            files1 = case mask of
                Nothing -> [ ]
                Just m -> [ FileData{..} ]
                  where
                    fdInputName = "mask"
                    fdFileName = Text.pack m
                    fdFileCType = "image/" <> getExtension m
                    fdPayload = m

-- | API
type API =
    "edits" :> MultipartForm Tmp CreateImageEdit :> Post '[JSON] (ListOf Image)