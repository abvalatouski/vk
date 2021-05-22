module Web.VK.Api.Events
  where

import           Data.Aeson             (FromJSON (parseJSON), (.:), (.:?))
import qualified Data.Aeson             as Json
import           Data.Text              (Text)

import           Web.VK.Api.CommonTypes

-- | See [VK documentation](https://vk.com/dev/groups_events).
data Event
    = MessageNew Message
    | OtherEvent Json.Object
  deriving stock Show

instance FromJSON Event where
    parseJSON = Json.withObject "event" \object -> do
        (eventType :: Text) <- object .: "type"
        if eventType == "message_new" then do
            eventObject <- object .: "object"
            message     <- eventObject .: "message"
            pure $ MessageNew message
        else
            pure $ OtherEvent object

-- | See [VK documentation](https://vk.com/dev/objects/message).
data Message = Message
    { messageId     :: Id
    , messagePeerId :: Id
    , messageFromId :: Id
    , messageText   :: Text
    , messageAction :: Maybe ServiceAction
    }
  deriving stock Show

instance FromJSON Message where
    parseJSON = Json.withObject "message" \object -> do
        messageId     <- object .:  "id"
        messagePeerId <- object .:  "peer_id"
        messageFromId <- object .:  "from_id"
        messageText   <- object .:  "text"
        messageAction <- object .:? "action"
        pure Message { .. }

-- | See [VK documentation](https://vk.com/dev/objects/message).
data ServiceAction
    = ChatInviteUserById Id
    | OtherServiceAction Json.Object
  deriving stock Show

instance FromJSON ServiceAction where
    parseJSON = Json.withObject "action" \object -> do
        (actionType :: Text) <- object .: "type"
        if actionType == "chat_invite_user" then do
            memberId <- object .:? "member_id"
            case memberId of
                Just id -> pure $ ChatInviteUserById id
                Nothing -> pure $ OtherServiceAction object
        else
            pure $ OtherServiceAction object
