{-# LANGUAGE DataKinds #-}

module Discord.API.Internal.Http.Common where
import Data.Text (Text)
import Network.HTTP.Req (runReq, defaultHttpConfig, GET (GET), req, jsonResponse, NoReqBody (NoReqBody), https, (/:), responseBody, Req, ReqBodyJson (ReqBodyJson), POST (POST), Scheme (Https), Url, HttpMethod (AllowsBody), ProvidesBody, HttpBodyAllowed, HttpResponse, HttpBody, DELETE (DELETE), PATCH (PATCH), PUT (PUT), (=:), QueryParam, Option, header, ignoreResponse, CanHaveBody (NoBody, CanHaveBody))
import Data.Aeson (FromJSON, ToJSON, encode)
import Web.HttpApiData (ToHttpApiData)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Data (Proxy)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.List (foldl')


baseUrl :: Url 'Https
baseUrl = https "discord.com" /: "api" /: "v9"

makeUrl :: [Text] -> Url 'Https
makeUrl = foldl' (/:) baseUrl

authHeader :: Text -> Option scheme
authHeader = header "Authorization" . ("Bot " <>) . encodeUtf8

(=:?) :: (QueryParam p, ToHttpApiData a, Monoid p) => Text -> Maybe a -> p
(=:?) name (Just value) = name =: value
(=:?) _ Nothing         = mempty


type AuthorizedRequest a = Text -> IO a


_requestApi :: (HttpMethod method, HttpResponse a, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpBody body) =>
    method -> [Text] -> body -> Proxy a -> Option 'Https -> AuthorizedRequest a
_requestApi method endpoint reqBodyType resBodyType options token = runReq defaultHttpConfig $ do
    req method (makeUrl endpoint) reqBodyType resBodyType (authHeader token <> options)



_getApi :: HttpResponse r => Proxy r -> [Text] -> Option 'Https -> AuthorizedRequest r
_getApi resBodyType endpoint = _requestApi GET endpoint NoReqBody resBodyType

getApi :: FromJSON a => [Text] -> Option 'Https -> AuthorizedRequest a
getApi endpoint options token = responseBody <$> _getApi jsonResponse endpoint options token

getApi_ :: [Text] -> Option 'Https -> AuthorizedRequest ()
getApi_ endpoint options token = _getApi ignoreResponse endpoint options token >> pure ()


_deleteApi :: HttpResponse r => Proxy r -> [Text] -> AuthorizedRequest r
_deleteApi resBodyType endpoint = _requestApi DELETE endpoint NoReqBody resBodyType mempty

deleteApi :: FromJSON a => [Text] -> AuthorizedRequest a
deleteApi endpoint token = responseBody <$> _deleteApi jsonResponse endpoint token

deleteApi_ :: [Text] -> AuthorizedRequest ()
deleteApi_ endpoint token = _deleteApi ignoreResponse endpoint token >> pure ()


_postApi :: (HttpResponse r, HttpBody body) =>
     [Text] -> body -> Proxy r -> AuthorizedRequest r
_postApi endpoint reqBody resBody =
    _requestApi POST endpoint reqBody resBody mempty

postApi :: (HttpBody body, FromJSON b) => [Text] -> body -> AuthorizedRequest b
postApi endpoint reqBody token = responseBody <$> _postApi endpoint reqBody jsonResponse token

postApi_ :: HttpBody body => [Text] -> body -> AuthorizedRequest ()
postApi_ endpoint reqBody token = _postApi endpoint reqBody ignoreResponse token >> pure ()


_patchApi :: (HttpResponse r, HttpBody body) =>
     [Text] -> body -> Proxy r -> AuthorizedRequest r
_patchApi endpoint reqBody resBody =
    _requestApi PATCH endpoint reqBody resBody mempty

patchApi :: (HttpBody body, FromJSON b) => [Text] -> body -> AuthorizedRequest b
patchApi endpoint reqBody token = responseBody <$> _patchApi endpoint reqBody jsonResponse token

patchApi_ :: HttpBody body => [Text] -> body -> AuthorizedRequest ()
patchApi_ endpoint reqBody token = _patchApi endpoint reqBody ignoreResponse token >> pure ()



_putApi :: (HttpResponse r, HttpBody body) =>
     [Text] -> body -> Proxy r -> AuthorizedRequest r
_putApi endpoint reqBody resBody =
    _requestApi PUT endpoint reqBody resBody mempty

putApi :: (HttpBody body, FromJSON b) => [Text] -> body -> AuthorizedRequest b
putApi endpoint reqBody token = responseBody <$> _putApi endpoint reqBody jsonResponse token

putApi_ :: HttpBody body => [Text] -> body -> AuthorizedRequest ()
putApi_ endpoint reqBody token = _putApi endpoint reqBody ignoreResponse token >> pure ()


