module Discord.Core.Embeds.Builder
( runEmbedBuilder
, author
, title
, url
, thumbnail
, description
, field
, image
, footer
, color
, timestamp
, authorUrl
, authorIconUrl
, authorProxyIconUrl
, footerIconUrl
, footerProxyIconUrl ) where

import Discord.API.Internal.Types.Embed (Embed (..), EmbedAuthor (EmbedAuthor, embedAuthorName, embedAuthorUrl, embedAuthorIconUrl, embedAuthorProxyIconUrl), EmbedThumbnail (EmbedThumbnail, embedThumbnailProxyUrl, embedThumbnailUrl, embedThumbnailHeight, embedThumbnailWidth), EmbedField (EmbedField), EmbedImage (EmbedImage, embedImageUrl, embedImageProxyUrl, embedImageHeight, embedImageWidth), EmbedFooter (EmbedFooter, embedFooterText, embedFooterIconUrl, embedFooterProxyIconUrl), EmbedVideo (EmbedVideo, embedVideoProxyUrl, embedVideoUrl, embedVideoHeight, embedVideoWidth), EmbedProvider (embedProviderName, embedProviderUrl, EmbedProvider), EmbedColor)
import Control.Monad.RWS (MonadState)
import Control.Monad.State (State, runState, modify)
import Data.Text (Text)
import Data.Time (UTCTime)



emptyEmbed :: Embed
emptyEmbed = Embed { embedAuthor      = Nothing
                   , embedTitle       = Nothing 
                   , embedUrl         = Nothing 
                   , embedThumbnail   = Nothing
                   , embedDescription = Nothing
                   , embedFields      = []
                   , embedImage       = Nothing
                   , embedFooter      = Nothing
                   , embedColor       = Nothing
                   , embedTimestamp   = Nothing
                   , embedVideo       = Nothing
                   , embedProvider    = Nothing 
                   }


newtype EmbedBuilderM a = EmbedBuilderM { _runEmbedBuilder :: State Embed a }
    deriving (Functor, Applicative, Monad, MonadState Embed)


{- | Defines a clean way of constructing 'Embed' values, which can have many optional fields, given a 'EmbedBuilderM'.

__Example__:

@
embedExample :: Embed
embedExample = runEmbedBuilder $ do 
    title "A nice title"
    description "a nicer embed description"
@
-}
runEmbedBuilder :: EmbedBuilderM () -> Embed
runEmbedBuilder = snd . (`runState` emptyEmbed) . _runEmbedBuilder


-- | Defines a clean way of constructing 'EmbedAuthor' values, which can have many optional fields, given a 'EmbedAuthorBuilderM'.
author 
    :: Text                    -- ^ Author name
    -> EmbedAuthorBuilderM ()  -- ^ Builder for optional fields
    -> EmbedBuilderM ()
author name authorBuilder = modify (\emb -> emb { embedAuthor = Just newAuthor })
    where newAuthor = runEmbedAuthorBuilder name authorBuilder


-- | Adds a title to the 'Embed' being built.
title 
    :: Text  -- ^ Title
    -> EmbedBuilderM ()
title t = modify (\emb -> emb { embedTitle = Just t })


-- | Adds an url to the 'Embed' being built.
url 
    :: Text  -- ^ Url
    -> EmbedBuilderM ()
url u = modify (\emb -> emb { embedUrl = Just u })


-- | Adds a thumbnail to the 'Embed' being built.
thumbnail 
    :: Text  -- ^ Image url
    -> EmbedBuilderM ()
thumbnail url = modify (\emb -> emb { embedThumbnail = Just newThumbnail })
    where newThumbnail = EmbedThumbnail { embedThumbnailUrl      = url
                                        , embedThumbnailProxyUrl = Nothing
                                        , embedThumbnailHeight   = Nothing
                                        , embedThumbnailWidth    = Nothing 
                                        } 


-- | Adds a description to the 'Embed' being built.
description 
    :: Text  -- ^ Description
    -> EmbedBuilderM ()
description desc = modify (\emb -> emb { embedDescription = Just desc })


-- | Adds a field to the 'Embed' being built.
field
    :: Text  -- ^ Field name
    -> Text  -- ^ Field value
    -> Bool  -- ^ Inline?
    -> EmbedBuilderM ()
field name value inline = modify $ appendField newField
    where newField = EmbedField name value inline
          appendField field embed = embed { embedFields = embedFields embed ++ [field] } 


-- | Adds an image to the 'Embed' being built.
image 
    :: Text  -- ^ Image url
    -> EmbedBuilderM ()
image url = modify (\emb -> emb { embedImage = Just newImage })
    where newImage = EmbedImage { embedImageUrl      = url
                                , embedImageProxyUrl = Nothing
                                , embedImageHeight   = Nothing
                                , embedImageWidth    = Nothing
                                }


-- | Defines a clean way of constructing 'EmbedFooter' values, which can have many optional fields, given a 'EmbedFooterBuilderM'.
footer 
    :: Text                    -- ^ Footer text
    -> EmbedFooterBuilderM ()  -- ^ Builder for optional fields
    -> EmbedBuilderM ()
footer txt footerBuilder = modify (\emb -> emb { embedFooter = Just newFooter })
    where newFooter = runEmbedFooterBuilder txt footerBuilder 


-- | Adds color to the 'Embed' being built.
color 
    :: EmbedColor  -- ^ Embed color
    -> EmbedBuilderM ()
color c = modify (\emb -> emb { embedColor = Just c })


-- | Adds a timestamp to the 'Embed' being built.
timestamp 
    :: UTCTime  -- ^ Timestamp
    -> EmbedBuilderM ()
timestamp t = modify (\emb -> emb { embedTimestamp = Just t })


{- Unsupported -}

-- video :: Text -> EmbedVideoBuilderM () -> EmbedBuilderM ()
-- video url videoBuilder = modify (\emb -> emb { embedVideo = Just newVideo })
--     where newVideo = runEmbedVideoBuilder url videoBuilder
-- 
-- provider :: Text -> Text -> EmbedBuilderM ()
-- provider name url = modify (\emb -> emb { embedProvider = Just newProvider })
--     where newProvider = EmbedProvider { embedProviderName = Just name
--                                       , embedProviderUrl  = Just url
--                                       }


emptyAuthor :: Text -> EmbedAuthor
emptyAuthor name = EmbedAuthor { embedAuthorName         = name
                               , embedAuthorUrl          = Nothing 
                               , embedAuthorIconUrl      = Nothing 
                               , embedAuthorProxyIconUrl = Nothing
                               }

newtype EmbedAuthorBuilderM a = EmbedAuthorBuilderM { _runEmbedAuthorBuilder :: State EmbedAuthor a }
    deriving (Functor, Applicative, Monad, MonadState EmbedAuthor)


runEmbedAuthorBuilder :: Text -> EmbedAuthorBuilderM () -> EmbedAuthor
runEmbedAuthorBuilder name = snd . (`runState` emptyAuthor name) . _runEmbedAuthorBuilder


-- | Adds an url to the 'EmbedAuthor' being built.
authorUrl 
    :: Text  -- ^ Author url
    -> EmbedAuthorBuilderM ()
authorUrl u = modify (\a -> a { embedAuthorUrl = Just u })


-- | Adds an author icon to the 'EmbedAuthor' being built.
authorIconUrl 
    :: Text  -- ^ Icon url
    -> EmbedAuthorBuilderM ()
authorIconUrl u = modify (\a -> a { embedAuthorIconUrl = Just u })


-- | Adds an author icon proxy to the 'EmbedAuthor' being built.
authorProxyIconUrl 
    :: Text  -- ^ Icon proxy url
    -> EmbedAuthorBuilderM ()
authorProxyIconUrl u = modify (\a -> a { embedAuthorProxyIconUrl = Just u })


{- Values other than url are unsupported for thumbnails -}

-- emptyThumbnail :: Text -> EmbedThumbnail
-- emptyThumbnail url = EmbedThumbnail { embedThumbnailUrl      = url
--                                     , embedThumbnailProxyUrl = Nothing
--                                     , embedThumbnailHeight   = Nothing
--                                     , embedThumbnailWidth    = Nothing
--                                     }
-- 
-- newtype EmbedThumbnailBuilderM a = EmbedThumbnailBuilderM { _runEmbedThumbnailBuilder :: State EmbedThumbnail a }
--     deriving (Functor, Applicative, Monad, MonadState EmbedThumbnail)
-- 
-- runEmbedThumbnailBuilder :: Text -> EmbedThumbnailBuilderM () -> EmbedThumbnail
-- runEmbedThumbnailBuilder u = snd . (`runState` emptyThumbnail u) . _runEmbedThumbnailBuilder
-- 
-- thumbnailProxyUrl :: Text -> EmbedThumbnailBuilderM ()
-- thumbnailProxyUrl url = modify (\thumb -> thumb { embedThumbnailProxyUrl = Just url })
-- 
-- thumbnailHeight :: Integer -> EmbedThumbnailBuilderM ()
-- thumbnailHeight height = modify (\thumb -> thumb { embedThumbnailHeight = Just height })
-- 
-- thumbnailWidth :: Integer -> EmbedThumbnailBuilderM ()
-- thumbnailWidth width = modify (\thumb -> thumb { embedThumbnailWidth = Just width })


{- Values other than url are unsupported for images -}

-- emptyImage :: Text -> EmbedImage
-- emptyImage url = EmbedImage { embedImageUrl      = url
--                             , embedImageProxyUrl = Nothing
--                             , embedImageHeight   = Nothing
--                             , embedImageWidth    = Nothing
--                             }
-- 
-- newtype EmbedImageBuilderM a = EmbedImageBuilderM { _runEmbedImageBuilder :: State EmbedImage a }
--     deriving (Functor, Applicative, Monad, MonadState EmbedImage)
-- 
-- runEmbedImageBuilder :: Text -> EmbedImageBuilderM () -> EmbedImage
-- runEmbedImageBuilder u = snd . (`runState` emptyImage u) . _runEmbedImageBuilder
-- 
-- imageProxyUrl :: Text -> EmbedImageBuilderM ()
-- imageProxyUrl url = modify (\img -> img { embedImageProxyUrl = Just url })
-- 
-- imageHeight :: Integer -> EmbedImageBuilderM ()
-- imageHeight height = modify (\img -> img { embedImageHeight = Just height })
-- 
-- imageWidth :: Integer -> EmbedImageBuilderM ()
-- imageWidth width = modify (\img -> img { embedImageWidth = Just width })



emptyFooter :: Text -> EmbedFooter
emptyFooter txt = EmbedFooter { embedFooterText         = txt
                              , embedFooterIconUrl      = Nothing
                              , embedFooterProxyIconUrl = Nothing
                              }

newtype EmbedFooterBuilderM a = EmbedFooterBuilderM { _runEmbedFooterBuilder :: State EmbedFooter a }
    deriving (Functor, Applicative, Monad, MonadState EmbedFooter)

runEmbedFooterBuilder :: Text -> EmbedFooterBuilderM () -> EmbedFooter
runEmbedFooterBuilder txt = snd . (`runState` emptyFooter txt) . _runEmbedFooterBuilder


-- | Adds an icon to the 'EmbedFooter' being built.
footerIconUrl 
    :: Text  -- ^ Icon url
    -> EmbedFooterBuilderM ()
footerIconUrl url = modify (\f -> f { embedFooterIconUrl = Just url })


-- | Adds an icon proxy to the 'EmbedFooter' being built.
footerProxyIconUrl 
    :: Text  -- ^ Icon proxy url
    -> EmbedFooterBuilderM ()
footerProxyIconUrl url = modify (\f -> f { embedFooterProxyIconUrl = Just url })



{- Unsupported -}

-- emptyVideo :: Text -> EmbedVideo
-- emptyVideo url = EmbedVideo { embedVideoUrl = Just url
--                             , embedVideoProxyUrl = Nothing
--                             , embedVideoHeight   = Nothing
--                             , embedVideoWidth    = Nothing
--                             }
-- 
-- newtype EmbedVideoBuilderM a = EmbedVideoBuilderM { _runEmbedVideoBuilder :: State EmbedVideo a }
--     deriving (Functor, Applicative, Monad, MonadState EmbedVideo)
-- 
-- runEmbedVideoBuilder :: Text -> EmbedVideoBuilderM () -> EmbedVideo
-- runEmbedVideoBuilder u = snd . (`runState` emptyVideo u) . _runEmbedVideoBuilder
-- 
-- videoProxyUrl :: Text -> EmbedVideoBuilderM ()
-- videoProxyUrl url = modify (\vid -> vid { embedVideoProxyUrl = Just url })
-- 
-- videoHeight :: Integer -> EmbedVideoBuilderM ()
-- videoHeight height = modify (\vid -> vid { embedVideoHeight = Just height })
-- 
-- videoWidth :: Integer -> EmbedVideoBuilderM ()
-- videoWidth width = modify (\vid -> vid { embedVideoWidth = Just width })