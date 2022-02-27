module Discord.Core.Context
( Context(..) ) where

import Data.Text (Text)
import Discord.API.Internal.Types.Message (Message (messageText))


-- | Context used during exception handling.
data Context =
    NoCtx                             -- ^ Exception ocurred with no context
    | MessageCtx Message              -- ^ Exception ocurred during 'onMessage'
    | CommandCtx Text [Text] Message  -- ^ Exception ocurred during 'onCommand'
    deriving Show