module Discord.API.Internal.Types.Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData)


newtype Snowflake = Snowflake Text deriving (Show, Eq, Generic, FromJSON, ToJSON, ToHttpApiData)

newtype ImageHash = ImageHash Text deriving (Show, Eq, Generic, FromJSON)

