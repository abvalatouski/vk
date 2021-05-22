module Web.VK.Api.LongPoll.Mtl
    ( -- * MTL-style interface
      MonadLongPoll (askLongPollServer)
    , LongPollT (LongPollT, runLongPollT)
    , LongPollM
    , runLongPoll

      -- * Server
    , LongPollServer
    , getLongPollServer

      -- * Events
    , awaitEvents
    , Event (MessageNew, OtherEvent)
      -- ** Helper types
    , Message (Message, messageId, messageFromId, messagePeerId, messageText, messageAction)
    , ServiceAction (ChatInviteUserById, OtherServiceAction)
    )
  where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Web.VK.Api
import           Web.VK.Api.LongPoll        hiding (getLongPollServer, awaitEvents)
import qualified Web.VK.Api.LongPoll        as Inner

import           Web.VK.Api.CommonTypes.Mtl

-- | Describes a monad capable of interacting with Long Poll API.
class MonadApi m => MonadLongPoll m where
    -- | Acquires Long Poll server from the context.
    askLongPollServer :: m LongPollServer

-- | Default Long Poll API transformer.
newtype LongPollT m a = LongPollT
    { runLongPollT :: ReaderT LongPollServer m a
    }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    )

instance MonadTrans LongPollT where
    lift = LongPollT . lift

instance MonadApi m => MonadApi (LongPollT m) where
    askApiConn = lift askApiConn

instance MonadApi m => MonadLongPoll (LongPollT m) where
    askLongPollServer = LongPollT ask

-- | Default Long Poll monad.
type LongPollM = LongPollT ApiM

-- | Runs an action in the context of Long Poll API.
{-# ANN runLongPoll ("HLint: ignore" :: String) #-}
runLongPoll :: (MonadIO m, MonadCatch m, MonadThrow m) => LongPollT m a -> ApiConn -> Id -> m a
runLongPoll action conn groupId = flip runApi conn do
    server <- getLongPollServer groupId
    lift $ runReaderT (runLongPollT action) server

-- | See [VK documentation](https://vk.com/dev/messages.getLongPollServer).
getLongPollServer :: MonadApi m => Id -> m LongPollServer
getLongPollServer groupId = do
    conn <- askApiConn
    liftIO $ Inner.getLongPollServer groupId conn

-- | Blocks and waits until the API will send new 'Event's or the waiting time will expire.
awaitEvents :: MonadLongPoll m => m [Event]
awaitEvents = do
    conn   <- askApiConn
    server <- askLongPollServer
    liftIO $ Inner.awaitEvents conn server
