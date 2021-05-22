module Web.VK.Api
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

import           Web.VK.Api.CommonTypes
import           Web.VK.Api.Method
import           Web.VK.Api.Param
