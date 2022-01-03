module Discord.API.Internal.Types.Embed where

import Data.Aeson
    ( (.!=), (.:), (.:?), withObject, FromJSON(parseJSON) )
import Data.Text (Text)
import Data.Time (UTCTime)


data Embed = Embed 
    { embedAuthor      :: Maybe EmbedAuthor     -- author information
    , embedTitle       :: Maybe Text            -- title of embed
    , embedUrl         :: Maybe Text            -- url of embed
    , embedThumbnail   :: Maybe EmbedThumbnail  -- thumbnail information
    , embedDescription :: Maybe Text            -- description of embed
    , embedFields      :: [EmbedField]          -- fields information
    , embedImage       :: Maybe EmbedImage      -- image information
    , embedFooter      :: Maybe EmbedFooter     -- footer information
    , embedColor       :: Maybe Integer         -- color code of the embed
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

data EmbedField = EmbedField
    { embedFieldName     :: Text        -- name of the field
    , embedFieldValue    :: Text        -- value of the field
    , embedFieldIsInline :: Maybe Bool  -- whether or not this field should display inline
    } deriving Show

instance FromJSON EmbedField where
    parseJSON = withObject "EmbedFiled" $ \o ->
        EmbedField <$> o .:  "name"
                   <*> o .:  "value"
                   <*> o .:? "inline"

data EmbedImage = EmbedImage
    { embedImageUrl :: Text              -- source url of image (only supports https(s) and attachments)
    , embedImageProxyUrl :: Maybe Text   -- a proxied url of the image
    , embedImageHeight :: Maybe Integer  -- height of image
    , embedImageWidth :: Maybe Integer   -- width of image
    } deriving Show

instance FromJSON EmbedImage where
    parseJSON = withObject "EmbedImage" $ \o ->
        EmbedImage <$> o .:  "url"
                   <*> o .:? "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"

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

data EmbedProvider = EmbedProvider
    { embedProviderName :: Maybe Text  -- name of provider
    , embedProviderUrl  :: Maybe Text  -- url of provider
    } deriving Show

instance FromJSON EmbedProvider where
    parseJSON = withObject "EmbedProvider" $ \o ->
        EmbedProvider <$> o .:? "name"
                      <*> o .:? "url"
