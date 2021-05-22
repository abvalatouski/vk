module Web.VK.Api.Mtl
    ( -- * Common types
      -- ** API connection
      ApiConn
    , Port
    , mkApiConn
    , mkApiConnDefault
      -- ** Others
    , ApiToken
    , ApiVersion
    , Id
    , randomId
    , UnhandledApiResponse (UnhandledApiResponse)

      -- * MTL-style interface
    , MonadApi (askApiConn)
    , ApiT (ApiT, runApiT)
    , ApiM
    , runApi

      -- * Calling API methods
    , Param
    , (@=)
    , callMethod
    , callMethod_
    , MethodError (MethodError, errorCode, errorMessage, errorPayload)
      -- ** Encoding API parameters
    , Encode (encode)
    , Encoded (Encode, getEncoded)
    , WrappedString (WrapString, getWrappedString)
    , Showed (Show, getShowed)
    )
  where

import           Control.Monad.IO.Class
import           Data.Aeson                 (FromJSON)
import           Data.Text                  (Text)
import           Web.VK.Api                 hiding (randomId, callMethod, callMethod_)
import qualified Web.VK.Api                 as Inner

import           Web.VK.Api.CommonTypes.Mtl

randomId :: MonadIO m => m Id
randomId = liftIO Inner.randomId

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
