import           Control.Monad

import           Control.Monad.Catch
import           Control.Monad.State
import           Data.FileEmbed          (embedStringFile)
import           Data.HashMap.Strict     (HashMap, (!))
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text)
import           Lens.Micro
import           Lens.Micro.TH
import           Web.VK.Api.Mtl          ((@=))
import qualified Web.VK.Api.Mtl          as VK
import qualified Web.VK.Api.LongPoll.Mtl as VK

data BotStatus = Active | Sleeping
  deriving stock Show

newtype BotState = BotState
    { _botStatus :: BotStatus
    }
  deriving stock Show

makeLenses ''BotState

data Conversation = Conversation
    { _conversationId       :: !VK.Id
    , _conversationBotState :: !BotState
    }
  deriving stock Show

makeLenses ''Conversation

type Conversations = HashMap VK.Id Conversation

conversation :: VK.Id -> Lens' Conversations Conversation
conversation id = lens (! id) (flip $ HashMap.insert id)

newtype BotM a = BotM
    { runBotM :: StateT Conversations VK.LongPollM a
    }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Conversations
    , MonadThrow
    , MonadCatch
    )

instance VK.MonadApi BotM where
    askApiConn = BotM $ lift VK.askApiConn

instance VK.MonadLongPoll BotM where
    askLongPollServer = BotM $ lift VK.askLongPollServer

main :: IO ()
main = runBot bot

runBot :: BotM a -> IO a
runBot action = do
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        groupId    = $(embedStringFile "private/group-id")
    conn <- VK.mkApiConnDefault apiToken apiVersion
    VK.runLongPollApi (evalStateT (runBotM action) []) groupId conn

bot :: BotM ()
bot = do
    events <- VK.awaitEvents
    forM_ events \case 
        VK.MessageNew VK.Message { .. } -> do
            groupId <- VK.askGroupId
            case messageAction of
                -- Yay! We've just been invited!
                Just (VK.ChatInviteUserById id) | id == -groupId -> do
                    let greeting = "Hello!"
                    sendMessage greeting messagePeerId
                _ ->
                    pure ()
        _ ->
            pure ()

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> BotM ()
sendMessage text userId = do
    randomId <- VK.randomId
    VK.callMethod_ "messages.send"
        [ "peer_id"   @= userId
        , "random_id" @= randomId
        , "message"   @= text
        ]
