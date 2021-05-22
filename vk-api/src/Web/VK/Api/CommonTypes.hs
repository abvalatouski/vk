module Web.VK.Api.CommonTypes
  where

import           Control.Concurrent
import           Data.Int
import           Data.Maybe
import           Data.String
import           Text.Read

import           Data.Aeson              (FromJSON)
import qualified Data.ByteString.Builder as BS
import           Data.List.Split
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Network.HTTP.Client     as Net
import qualified Network.HTTP.Client.TLS as Net
import           System.Random

import           Web.VK.Api.Param

-- | Describes the application and its permissions.
newtype ApiToken = ApiToken
    { getApiToken :: Text
    }
  deriving stock   Eq
  deriving newtype Encode

instance Show ApiToken where
    show = Text.unpack . getApiToken

instance Read ApiToken where
    readsPrec _precedence input =
        let remainingInput = ""
         in [(ApiToken $ Text.pack input, remainingInput)]

instance IsString ApiToken where
    fromString = read

-- | API version.
--
--   See [VK documentation](https://vk.com/dev/versions).
data ApiVersion = ApiVersion
    { majorVersion :: !Int
    , minorVersion :: !Int
    }
  deriving stock (Eq, Ord)

instance Show ApiVersion where
    show (ApiVersion major minor) = show major <> "." <> show minor

instance Read ApiVersion where
    readsPrec _precedence input = maybeToList do
        [major, minor] <- traverse readMaybe $ splitOn "." input
        let remainingInput = ""
        pure (ApiVersion major minor, remainingInput)

instance IsString ApiVersion where
    fromString = read

instance Encode ApiVersion where
    encode (ApiVersion major minor) = encode major <> BS.char8 '.' <> encode minor

newtype Id = Id
    { getId :: Int32
    }
  deriving newtype
    ( Show
    , Read
    , Eq
    , Ord
    , Num
    , Encode
    , Random
    , FromJSON
    )

instance IsString Id where
    fromString = read

randomId :: IO Id
randomId = randomIO

-- | Contains data about /authorized/ API connection.
data ApiConn = ApiConn
    { apiConnPort    :: !Port
    , apiConnManager :: !Net.Manager
    , apiConnLocker  :: !(MVar ())
    , apiToken       :: !ApiToken
    , apiVersion     :: !ApiVersion
    }

newtype Port = Port
    { getPort :: Int
    }
  deriving newtype (Show, Read, Eq, Ord)

-- | Smart constructor.
mkApiConn :: Port -> Net.Manager -> ApiToken -> ApiVersion -> IO ApiConn
mkApiConn port manager token version = do
    locker <- newMVar ()
    pure $ ApiConn port manager locker token version

-- | Smart constructor.
--
--   The default port is 443. The default 'Net.Manager' is TLS-manager.
mkApiConnDefault :: ApiToken -> ApiVersion -> IO ApiConn
mkApiConnDefault token version = do
    manager <- Net.newTlsManager
    mkApiConn (Port 443) manager token version
