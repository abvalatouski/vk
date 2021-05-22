{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Data.FileEmbed (embedStringFile)
import           Data.Text      (Text)
import           Web.VK.Api.Mtl ((@=))
import qualified Web.VK.Api.Mtl as VK

main :: IO ()
main = do
    -- Don't forget to create "private" directory with your secrets.
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
    VK.runApi bot =<< VK.mkApiConnDefault apiToken apiVersion

bot :: VK.ApiM ()
bot = do
    -- Don't forget to create "private" directory with your secrets.
    let me = $(embedStringFile "private/my-id")
    sendMessage "Hello, world!" me

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> VK.ApiM ()
sendMessage text userId = do
    randomId <- VK.randomId
    VK.callMethod_ "messages.send"
        [ "peer_id"   @= userId
        , "random_id" @= randomId
        , "message"   @= text
        ]
