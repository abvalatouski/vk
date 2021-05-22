module Web.VK.Api.LongPoll
    ( -- * Server
      LongPollServer
    , getLongPollServer

      -- * Events
    , awaitEvents
    , Event (MessageNew, OtherEvent)
      -- ** Helper types
    , Message (Message, messageId, messageFromId, messagePeerId, messageText, messageAction)
    , ServiceAction (ChatInviteUserById, OtherServiceAction)
    )
  where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.IORef

import           Data.Aeson             (FromJSON (parseJSON), (.:), (.:?))
import qualified Data.Aeson             as Json
import           Data.Attoparsec.Text   (Parser, parseOnly)
import qualified Data.Attoparsec.Text   as Parse
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import qualified Network.HTTP.Client    as Net

import           Web.VK.Api.CommonTypes
import           Web.VK.Api.Events
import           Web.VK.Api.Method
import           Web.VK.Api.Param

-- Obtaining Long Poll API server

data Server' = Server'
    { serverKey       :: !Text
    , serverURL       :: !Url
    , serverTimestamp :: !Timestamp
    }
  deriving stock Show

instance FromJSON Server' where
    parseJSON = Json.withObject "Long Poll API server" \object -> do
        serverKey       <- object .: "key"
        serverURL       <- object .: "server"
        serverTimestamp <- object .: "ts"
        pure Server' { .. }

data Url = Url
    { urlHost :: !Text
    , urlPath :: !Text
    }
  deriving stock Show

instance FromJSON Url where
    parseJSON = Json.withText "Long Poll API server URL" $ maybe empty pure . parse url

url :: Parser Url
url = do
    Parse.string "https://"
    urlHost <- Parse.takeTill (== '/')
    urlPath <- Parse.takeText
    pure Url { .. }

newtype Timestamp = Timestamp
    { getTimestamp :: Text
    }
  deriving stock   Show
  deriving newtype (FromJSON, Encode)

-- See https://vk.com/dev/groups.getLongPollServer.
getLongPollServer' :: Id -> ApiConn -> IO Server'
getLongPollServer' groupId = callMethod "groups.getLongPollServer" ["group_id" @= groupId]

-- | See [VK documentation](https://vk.com/dev/bots_longpoll).
data LongPollServer = LongPollServer
    { longPollGroupId :: !Id
    , longPollState   :: !(IORef Server')
    }

-- | See [VK documentation](https://vk.com/dev/messages.getLongPollServer).
getLongPollServer :: Id -> ApiConn -> IO LongPollServer
getLongPollServer groupId conn = do
    state  <- newIORef =<< getLongPollServer' groupId conn
    pure $ LongPollServer groupId state

-- Checking for events.

data Update
    = Update
        { newTimestamp :: !Timestamp
        , newEvents    :: [Event]
        }
    | NeedToUpdateServer
    | NeedToUpdateTimestamp !Timestamp
  deriving stock Show

instance FromJSON Update where
    parseJSON = Json.withObject "Long Poll API update" \object -> do
        errorCode <- object .:? "failed"
        case errorCode of
            Nothing -> do
                newTimestamp <- object .: "ts"
                newEvents    <- object .: "updates"
                pure Update { .. }
            Just (errorCode :: Int) ->
                if errorCode == 1 then do
                    newTimestamp <- object .: "ts"
                    pure $ NeedToUpdateTimestamp newTimestamp
                else
                    pure NeedToUpdateServer

-- | Blocks and waits until the API will send new 'Event's or the waiting time will expire.
awaitEvents :: ApiConn -> LongPollServer -> IO [Event]
awaitEvents conn@ApiConn { .. } server@LongPollServer { .. } = do
    request  <- mkLongPollRequest conn <$> readIORef longPollState
    response <- withMVar apiConnLocker $ const $
        Net.responseBody <$> Net.httpLbs request apiConnManager
    case Json.decode' response of
        Just NeedToUpdateServer -> do
            writeIORef longPollState =<< getLongPollServer' longPollGroupId conn
            awaitEvents conn server -- Possible infinite loop!
        Just (NeedToUpdateTimestamp newTimestamp) -> do
            modifyIORef' longPollState \state -> state { serverTimestamp = newTimestamp }
            awaitEvents conn server -- Possible infinite loop!
        Just Update { .. } -> do
            modifyIORef' longPollState \state -> state { serverTimestamp = newTimestamp }
            pure newEvents
        Nothing ->
            throw $ UnhandledApiResponse response

mkLongPollRequest :: ApiConn -> Server' -> Net.Request
mkLongPollRequest ApiConn { .. } Server' { .. } =
    let params =
            [ "act"  @= ("a_check" :: Text)
            , "key"  @= serverKey
            , "ts"   @= serverTimestamp
            , "wait" @= (25 :: Int) -- API recommendation.
            ]
     in Net.defaultRequest
            { Net.secure      = True -- Use HTTPS.
            , Net.port        = getPort apiConnPort
            , Net.method      = "POST"
            , Net.host        = Text.encodeUtf8 $ urlHost serverURL
            , Net.path        = Text.encodeUtf8 $ urlPath serverURL
            , Net.requestBody = toPostRequestBody params
            }

-- Utils.

parse :: Parser a -> Text -> Maybe a
parse parser = either (const Nothing) Just . parseOnly (parser <* Parse.endOfInput)
