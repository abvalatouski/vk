module Web.VK.Api.Mtl
  where

import           Control.Monad.IO.Class
import           Data.Aeson                 (FromJSON)
import           Data.Text                  (Text)
import           Web.VK.Api                 (Param)
import qualified Web.VK.Api                 as Inner

import           Web.VK.Api.CommonTypes.Mtl

-- | Calls an API method with the given name and set of parameters.
--
--   See [VK documentation](https://vk.com/dev/methods) to find an appropriate method.
callMethod :: (MonadApi m, FromJSON a) => Text -> [Param] -> m a
callMethod name params = do
    conn <- askApiConn
    liftIO $ Inner.callMethod name params conn

-- | Acts like 'callMethod' but ignores the incoming API response.
callMethod_ :: MonadApi m => Text -> [Param] -> m ()
callMethod_ name params = do
    conn <- askApiConn
    liftIO $ Inner.callMethod_ name params conn
