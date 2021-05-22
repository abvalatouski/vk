{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Data.FileEmbed (embedStringFile)
import           Data.Text      (Text)
import           Web.VK.Api     ((@=))
import qualified Web.VK.Api     as VK

main :: IO ()
main = do
    -- Don't forget to create "private" directory with your secrets.
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        me      = $(embedStringFile "private/my-id")
    conn <- VK.mkApiConnDefault apiToken apiVersion
    sendMessage "Hello, world!" me conn

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> VK.ApiConn -> IO ()
sendMessage text userId conn = do
    randomId <- VK.randomId
    let method = "messages.send"
        params = ["peer_id" @= userId, "random_id" @= randomId, "message" @= text]
    VK.callMethod_ method params conn
