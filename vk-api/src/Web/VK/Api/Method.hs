module Web.VK.Api.Method
  where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Aeson                (FromJSON (parseJSON), (.:))
import qualified Data.Aeson                as Json
import qualified Data.Aeson.Types          as Json
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Builder   as BS
import qualified Data.ByteString.Lazy      as Lazy (ByteString)
import qualified Data.ByteString.Lazy      as Lbs
import qualified Data.HashMap.Strict       as HashMap
import           Data.Text                 (Text)
import qualified Network.HTTP.Client       as Net

import           Web.VK.Api.CommonTypes
import           Web.VK.Api.Param

-- Parsing API response.

data MethodResult a
    = MethodSuccess !a
    | MethodFailure MethodError
  deriving stock Show

instance FromJSON a => FromJSON (MethodResult a) where
    parseJSON = Json.withObject "API method response" \object ->
        let success = object .: "response"
            failure = object .: "error"
         in (MethodSuccess <$> success) <|> (MethodFailure <$> failure)

-- | Occurs on calling an API method.
--
--   See [VK documentation](https://vk.com/dev/errors).
data MethodError = MethodError
    { errorCode    :: !Int
    , errorMessage :: !Text
    , errorPayload :: Json.Object
    }
  deriving anyclass Exception

instance Show MethodError where
    show (MethodError code message payload) = unlines
        [ "MethodError"
        , "    { " <> show code
        , "    , " <> show message
        , "    , " <> show payload
        , "    }"
        ]

instance FromJSON MethodError where
    parseJSON = Json.withObject "API method error" $ evalStateT do
        errorCode    <- extractField "error_code"
        errorMessage <- extractField "error_msg"
        errorPayload <- get
        pure MethodError { .. }

-- Used to ignore any incoming JSON data.
data Ignored = Ignored
  deriving stock Show

instance FromJSON Ignored where
    parseJSON = const $ pure Ignored

-- Calling API methods.

-- | Calls an API method with the given name and set of parameters.
--
--   See [VK documentation](https://vk.com/dev/methods) to find an appropriate method.
callMethod :: FromJSON a => Text -> [Param] -> ApiConn -> IO a
callMethod name params conn = do
    let request = mkMethodRequest name params conn
    sendMethodRequest request conn

-- | Acts like 'callMethod' but ignores the incoming API response.
callMethod_ :: Text -> [Param] -> ApiConn -> IO ()
callMethod_ name params conn = do
    Ignored <- callMethod name params conn
    pure ()

sendMethodRequest :: FromJSON a => Net.Request -> ApiConn -> IO a
sendMethodRequest request ApiConn { .. } = do
    response <- withMVar apiConnLocker $ const $
        Net.responseBody <$> Net.httpLbs request apiConnManager
    case Json.decode' response of
        Just (MethodSuccess value)              -> pure value
        Just (MethodFailure MethodError { .. }) -> throw MethodError { .. }
        Nothing                                 -> throw $ UnhandledApiResponse response

mkMethodRequest :: Text -> [Param] -> ApiConn -> Net.Request
mkMethodRequest name params ApiConn { .. } =
    let params' = params <> ["access_token" @= apiToken, "v" @= apiVersion]
        path    = "/method/" <> encode name
     in Net.defaultRequest
            { Net.secure      = True -- Use HTTPS.
            , Net.port        = getPort apiConnPort
            , Net.method      = "POST"
            , Net.host        = "api.vk.com"
            , Net.path        = buildBS path
            , Net.requestBody = toPostRequestBody params'
            }

-- Utils.

extractField :: FromJSON a => Text -> StateT Json.Object Json.Parser a
extractField name = do
    object <- get
    value  <- lift $ object .: name
    modify' $ HashMap.delete name
    pure value

buildBS :: BS.Builder -> ByteString
buildBS = toStrictBS . BS.toLazyByteString

-- Should I update @bytestring@ package and use 'BS.toStrict' function.
toStrictBS :: Lazy.ByteString -> ByteString
toStrictBS = BS.concat . Lbs.toChunks
