{-# LANGUAGE DataKinds #-}
module Discord.API.Internal.Http.Common where
import Data.Text (Text)
import Network.HTTP.Req (runReq, defaultHttpConfig, GET (GET), req, jsonResponse, NoReqBody (NoReqBody), https, (/:), responseBody, Req, ReqBodyJson (ReqBodyJson), POST (POST), Scheme (Https), Url, HttpMethod (AllowsBody), ProvidesBody, HttpBodyAllowed, HttpResponse, HttpBody, DELETE (DELETE), PATCH (PATCH), PUT (PUT), (=:), QueryParam, Option, header)
import Data.Aeson (FromJSON, ToJSON)
import Web.HttpApiData (ToHttpApiData)
import Data.Text.Encoding (encodeUtf8)


baseUrl :: Url 'Https
baseUrl = https "discord.com" /: "api" /: "v9"

authHeader :: Text -> Option scheme
authHeader = header "Authorization" . ("Bot " <>) . encodeUtf8


(/+) :: Text -> Text -> Text
(/+) a b = a <> "/" <> b 


(=:?) :: (QueryParam p, ToHttpApiData a, Monoid p) => Text -> Maybe a -> p
(=:?) name (Just value) = name =: value
(=:?) _ Nothing = mempty


type AuthorizedRequest a = Text -> IO a


getApi :: FromJSON a => Text -> Option 'Https -> AuthorizedRequest a
getApi endpoint options token = runReq defaultHttpConfig $ do
    response <- req GET (baseUrl /: endpoint) NoReqBody jsonResponse (authHeader token <> options)
    pure $ responseBody response


deleteApi :: FromJSON a => Text -> AuthorizedRequest a
deleteApi endpoint token = runReq defaultHttpConfig $ do
    response <- req DELETE (baseUrl /: endpoint) NoReqBody jsonResponse (authHeader token)
    pure $ responseBody response


postApi :: (ToJSON a, FromJSON b) => Text -> a -> AuthorizedRequest b
postApi endpoint payload token = runReq defaultHttpConfig $ do
    response <- req POST (baseUrl /: endpoint) (ReqBodyJson payload) jsonResponse (authHeader token)
    pure $ responseBody response


patchApi :: (ToJSON a, FromJSON b) => Text -> a -> AuthorizedRequest b
patchApi endpoint payload token = runReq defaultHttpConfig $ do
    response <- req PATCH (baseUrl /: endpoint) (ReqBodyJson payload) jsonResponse (authHeader token)
    pure $ responseBody response


putApi :: (ToJSON a, FromJSON b) => Text -> a -> AuthorizedRequest b
putApi endpoint payload token = runReq defaultHttpConfig $ do
    response <- req PUT (baseUrl /: endpoint) (ReqBodyJson payload) jsonResponse (authHeader token)
    pure $ responseBody response


