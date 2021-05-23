module Web.VK.Api.LongPoll.Mtl
    ( -- * MTL-style interface
      MonadLongPoll (askLongPollServer)
    , askGroupId
    , LongPollT (LongPollT, runLongPollT)
    , LongPollM
    , runLongPollApi

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

-- | Acquires group ID from the context.
askGroupId :: MonadLongPoll m => m Id
askGroupId = longPollGroupId <$> askLongPollServer

-- | Default Long Poll API transformer.
newtype LongPollT m a = LongPollT
    { runLongPollT :: ReaderT LongPollServer m a
    }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
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
{-# ANN runLongPollApi ("HLint: Ignore" :: String) #-}
runLongPollApi ::
    (MonadIO m, MonadThrow m, MonadCatch m)
 => LongPollT (ApiT m) a -> Id -> ApiConn -> m a
runLongPollApi action groupId conn = flip runApi conn do
    server <- getLongPollServer groupId
    runReaderT (runLongPollT action) server

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
