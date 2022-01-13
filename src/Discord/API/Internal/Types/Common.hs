module Discord.API.Internal.Types.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import Data.Text (Text)
import GHC.Generics (Generic)


newtype Snowflake = Snowflake Text deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ImageHash = ImageHash Text deriving (Show, Eq, Generic, FromJSON)

