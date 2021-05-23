module ApiMethods
  where

import           Data.Text      (Text)
import           Web.VK.Api.Mtl ((@=))
import qualified Web.VK.Api.Mtl as VK

-- See "https://vk.com/dev/messages.send".
sendMessage :: VK.MonadApi m => Text -> VK.Id -> m ()
sendMessage text userId = do
    randomId <- VK.randomId
    VK.callMethod_ "messages.send"
        [ "peer_id"   @= userId
        , "random_id" @= randomId
        , "message"   @= text
        ]
