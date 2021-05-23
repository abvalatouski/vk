module Web.VK.Api.Param
  where

import           Data.Bool
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Monoid
import           Data.Word

import           Data.Aeson              (ToJSON)
import qualified Data.Aeson.Encoding     as Json
import qualified Data.ByteString.Builder as BS
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as Lazy (Text)
import qualified Data.Text.Lazy          as Lazy.Text
import qualified Data.Text.Lazy.Builder  as Text
import qualified Network.HTTP.Client     as Net
import qualified Network.URI             as Net

-- Is it good fixity for parameter "assignment"?
infix 1 @=

-- | Describes data that can be sent to the API as a query parameter or a query itself.
class Encode a where
    -- | Converts data to its textual representation before sending it to the API.
    encode :: a -> BS.Builder

instance Encode Bool where
    encode = BS.char8 . bool '0' '1'

instance Encode Char where
    encode = BS.string8 . Net.escapeURIChar isNotReserved

instance Encode Int where
    encode = BS.intDec

instance Encode Int8 where
    encode = BS.int8Dec

instance Encode Int16 where
    encode = BS.int16Dec

instance Encode Int32 where
    encode = BS.int32Dec

instance Encode Int64 where
    encode = BS.int64Dec

instance Encode Word where
    encode = BS.wordDec

instance Encode Word8 where
    encode = BS.word8Dec

instance Encode Word16 where
    encode = BS.word16Dec

instance Encode Word32 where
    encode = BS.word32Dec

instance Encode Word64 where
    encode = BS.word64Dec

instance Encode Float where
    encode = BS.floatDec

instance Encode Double where
    encode = BS.doubleDec

instance Encode Text where
    encode = foldMap encode . Text.unpack

instance Encode Lazy.Text where
    encode = foldMap encode . Lazy.Text.toChunks

instance Encode Text.Builder where
    encode = encode . Text.toLazyText

instance ToJSON tag => Encode (Json.Encoding' tag) where
    encode = Json.fromEncoding

instance Encode a => Encode (Maybe a) where
    encode = maybe mempty encode

instance (Encode a, Encode b) => Encode (Either a b) where
    encode = either encode encode

instance Encode a => Encode [a] where
    encode = encodeJoinedWith ','

-- API parameters.

-- | A parameter to the API. Consists of name and value.
data Param = Param !Text !BS.Builder
  deriving Show via Encoded Param

-- | Smart constructor.
(@=) :: Encode a => Text -> a -> Param
name @= value = Param name (encode value)

instance Encode Param where
    encode (Param name value) = encode name <> BS.char8 '=' <> value

newtype Params = Params
    { getParams :: [Param]
    }
  deriving Show via Encoded Params

instance Encode Params where
    encode = encodeJoinedWith '&' . getParams

toPostRequestBody :: [Param] -> Net.RequestBody
toPostRequestBody = Net.RequestBodyLBS . BS.toLazyByteString . encode . Params

-- Helper type wrappers.

-- | Used to derive 'Show'
--   [via](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) 'Encode'
--   instances.
newtype Encoded a = Encode
    { getEncoded :: a
    }

instance Encode a => Show (Encoded a) where
    show = stripQuotes . show . BS.toLazyByteString . encode . getEncoded
      where
        stripQuotes = tail . init

-- | Used to encode 'String' not as list of 'Char's but as solid text.
newtype WrappedString = WrapString
    { getWrappedString :: String
    }
  deriving Show via Encoded WrappedString

instance Encode WrappedString where
    encode = foldMap encode . getWrappedString

-- | Used to encode values via their 'Show' instances.
newtype Showed a = Show
    { getShowed :: a
    }
  deriving Show via Encoded (Showed a)

instance Show a => Encode (Showed a) where
    encode = foldMap encode . show . getShowed

-- Utils.

-- See "https://en.wikipedia.org/wiki/Percent-encoding".
isNotReserved :: Char -> Bool
isNotReserved = getAny . foldMap (Any .) predicates
  where
    predicates =
        [ isAsciiUpper
        , isAsciiLower
        , isDigit
        , (== '-')
        , (== '_')
        , (== '.')
        , (== '~')
        ]

encodeJoinedWith :: Encode a => Char -> [a] -> BS.Builder
encodeJoinedWith separator = mconcat . intersperse (BS.char8 separator) . fmap encode
