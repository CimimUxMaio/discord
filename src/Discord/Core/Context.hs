module Discord.Core.Context
( Context(..) ) where

import Data.Text (Text)
import Discord.API.Internal.Types.Message (Message (messageText))


data Context =
    NoCtx
    | MessageCtx Message
    | CommandCtx Text [Text] Message
    deriving Show