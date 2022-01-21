{-# LANGUAGE RecordWildCards #-}
module Discord.API.Internal.Types.Embed where

import Data.Aeson
    ( (.!=), (.:), (.:?), withObject, FromJSON(parseJSON), ToJSON (toJSON), object, (.=) )
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson.Types (ToJSON, Parser)


data Embed = Embed
    { embedAuthor      :: Maybe EmbedAuthor     -- author information
    , embedTitle       :: Maybe Text            -- title of embed
    , embedUrl         :: Maybe Text            -- url of embed
    , embedThumbnail   :: Maybe EmbedThumbnail  -- thumbnail information
    , embedDescription :: Maybe Text            -- description of embed
    , embedFields      :: [EmbedField]          -- fields information
    , embedImage       :: Maybe EmbedImage      -- image information
    , embedFooter      :: Maybe EmbedFooter     -- footer information
    , embedColor       :: Maybe (EmbedColor)    -- color code of the embed
    , embedTimestamp   :: Maybe UTCTime         -- timestamp of embed content
    , embedVideo       :: Maybe EmbedVideo      -- video information
    , embedProvider    :: Maybe EmbedProvider   -- provider information
    } deriving Show

instance FromJSON Embed where
    parseJSON = withObject "Embed" $ \o ->
        Embed <$> o .:? "author"
              <*> o .:? "title"
              <*> o .:? "url"
              <*> o .:? "thumbnail"
              <*> o .:? "description"
              <*> o .:? "fields" .!= []
              <*> o .:? "image"
              <*> o .:? "footer"
              <*> o .:? "color"
              <*> o .:? "timestamp"
              <*> o .:? "video"
              <*> o .:? "provider"

instance ToJSON Embed where
    toJSON Embed{..} =
        object [ "author"      .= embedAuthor
               , "title"       .= embedTitle
               , "url"         .= embedUrl
               , "thumbnail"   .= embedThumbnail
               , "description" .= embedDescription
               , "fields"      .= embedFields
               , "image"       .= embedImage
               , "footer"      .= embedFooter
               , "color"       .= embedColor
               , "timestamp"   .= embedTimestamp
               , "video"       .= embedVideo
               , "provider"    .= embedProvider
               ]


data EmbedAuthor = EmbedAuthor
    { embedAuthorName         :: Text        -- name of author
    , embedAuthorUrl          :: Maybe Text  -- url of author
    , embedAuthorIconUrl      :: Maybe Text  -- url of author icon (only supports http(s) and attachments)
    , embedAuthorProxyIconUrl :: Maybe Text  -- a proxied url of author icon
    } deriving Show

instance FromJSON EmbedAuthor where
    parseJSON = withObject "EmbedAuthor" $ \o ->
        EmbedAuthor <$> o .:  "name"
                    <*> o .:? "url"
                    <*> o .:? "icon_url"
                    <*> o .:? "proxy_icon_url"

instance ToJSON EmbedAuthor where
    toJSON EmbedAuthor{..} =
        object [ "name"           .= embedAuthorName
               , "url"            .= embedAuthorUrl
               , "url"            .= embedAuthorIconUrl
               , "proxy_icon_url" .= embedAuthorProxyIconUrl
               ]


data EmbedThumbnail = EmbedThumbnail
    { embedThumbnailUrl      :: Text           -- source url of thumbnail (only supports http(s) and attachments)
    , embedThumbnailProxyUrl :: Maybe Text     -- a proxied url of the thumbnail
    , embedThumbnailHeight   :: Maybe Integer  -- height of the thumbnail
    , embedThumbnailWidth    :: Maybe Integer  -- width of thumbnail
    } deriving Show

instance FromJSON EmbedThumbnail where
    parseJSON = withObject "EmbedThumbnail" $ \o ->
        EmbedThumbnail <$> o .:  "url"
                       <*> o .:? "proxy_url"
                       <*> o .:? "height"
                       <*> o .:? "width"

instance ToJSON EmbedThumbnail where
    toJSON EmbedThumbnail{..} =
        object [ "url"       .= embedThumbnailUrl
               , "proxy_url" .= embedThumbnailProxyUrl
               , "height"    .= embedThumbnailHeight
               , "width"     .= embedThumbnailWidth
               ]


data EmbedField = EmbedField
    { embedFieldName     :: Text  -- name of the field
    , embedFieldValue    :: Text  -- value of the field
    , embedFieldIsInline :: Bool  -- whether or not this field should display inline
    } deriving Show

instance FromJSON EmbedField where
    parseJSON = withObject "EmbedFiled" $ \o ->
        EmbedField <$> o .:  "name"
                   <*> o .:  "value"
                   <*> o .:? "inline" .!= False

instance ToJSON EmbedField where
    toJSON EmbedField{..} =
        object [ "name"   .= embedFieldName
               , "value"  .= embedFieldValue
               , "inline" .= embedFieldIsInline
               ]


data EmbedImage = EmbedImage
    { embedImageUrl      :: Text           -- source url of image (only supports https(s) and attachments)
    , embedImageProxyUrl :: Maybe Text     -- a proxied url of the image
    , embedImageHeight   :: Maybe Integer  -- height of image
    , embedImageWidth    :: Maybe Integer  -- width of image
    } deriving Show

instance FromJSON EmbedImage where
    parseJSON = withObject "EmbedImage" $ \o ->
        EmbedImage <$> o .:  "url"
                   <*> o .:? "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"

instance ToJSON EmbedImage where
    toJSON EmbedImage{..} =
        object [ "url"       .= embedImageUrl
               , "proxy_url" .= embedImageProxyUrl
               , "height"    .= embedImageHeight
               , "width"     .= embedImageWidth
               ]


data EmbedFooter = EmbedFooter
    { embedFooterText         :: Text        -- footer text
    , embedFooterIconUrl      :: Maybe Text  -- url of footer icon (only supports http(s) and attachments)
    , embedFooterProxyIconUrl :: Maybe Text  -- a proxied url of footer icon
    } deriving Show

instance FromJSON EmbedFooter where
    parseJSON = withObject "EmbedFooter" $ \o ->
        EmbedFooter <$> o .:  "text"
                    <*> o .:? "icon_url"
                    <*> o .:? "proxy_icon_url"

instance ToJSON EmbedFooter where
    toJSON EmbedFooter{..} =
        object [ "text"           .= embedFooterText
               , "icon_url"       .= embedFooterIconUrl
               , "proxy_icon_url" .= embedFooterProxyIconUrl
               ]


data EmbedColor = EmbedColor Integer Integer Integer
    deriving Show

instance FromJSON EmbedColor where
    parseJSON value = do
        decimalValue <- parseJSON value :: Parser Integer
        let r = decimalValue `div` (256 * 256)
        let g = (decimalValue `div` 256) `mod` 256
        let b = decimalValue `mod` 256
        pure (EmbedColor r g b)

instance ToJSON EmbedColor where
    toJSON (EmbedColor r g b) = toJSON (r * 256 * 256 + g * 256 + b)



data EmbedVideo = EmbedVideo
    { embedVideoUrl      :: Maybe Text     -- source url of video
    , embedVideoProxyUrl :: Maybe Text     -- a proxied url of the video
    , embedVideoHeight   :: Maybe Integer  -- height of video
    , embedVideoWidth    :: Maybe Integer  -- width of video
    } deriving Show

instance FromJSON EmbedVideo where
    parseJSON = withObject "EmbedVideo" $ \o ->
        EmbedVideo <$> o .:? "url"
                   <*> o .:? "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"

instance ToJSON EmbedVideo where
    toJSON EmbedVideo{..} =
        object [ "url"       .= embedVideoUrl
               , "proxy_url" .= embedVideoProxyUrl
               , "height"    .= embedVideoHeight
               , "width"     .= embedVideoWidth
               ]


data EmbedProvider = EmbedProvider
    { embedProviderName :: Maybe Text  -- name of provider
    , embedProviderUrl  :: Maybe Text  -- url of provider
    } deriving Show

instance FromJSON EmbedProvider where
    parseJSON = withObject "EmbedProvider" $ \o ->
        EmbedProvider <$> o .:? "name"
                      <*> o .:? "url"

instance ToJSON EmbedProvider where
    toJSON EmbedProvider{..} =
        object [ "name" .= embedProviderName
               , "url"  .= embedProviderUrl
               ]
