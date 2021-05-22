module Web.VK.Api.CommonTypes.Mtl
  where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON)
import           Data.Text            (Text)
import           Web.VK.Api

-- | Describes a monad capable of interacting with the API.
class (MonadIO m, MonadThrow m, MonadCatch m) => MonadApi m where
    -- | Acquires API connection from the context.
    askApiConn :: m ApiConn

-- | Default API monad transformer.
newtype ApiT m a = ApiT
    { runApiT :: ReaderT ApiConn m a
    }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

instance MonadTrans ApiT where
    lift = ApiT . lift

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadApi (ApiT m) where
    askApiConn = ApiT ask

-- | Default API monad.
type ApiM = ApiT IO

-- | Runs an action in the context of API.
runApi :: ApiT m a -> ApiConn -> m a
runApi = runReaderT . runApiT
