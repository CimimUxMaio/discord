module Discord.API.Internal.Types.Common where

import Data.Aeson (FromJSON)
import Data.Word (Word64)
import Data.Text (Text)
import GHC.Generics (Generic)


newtype Snowflake = Snowflake Word64 deriving (Show, Eq, Generic, FromJSON)

newtype ImageHash = ImageHash Text deriving (Show, Eq, Generic, FromJSON)

