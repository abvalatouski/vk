# Dealing with VKontakte API

## Example 1. Hello, world!

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Data.FileEmbed (embedStringFile)
import           Data.Text      (Text)
import           Web.VK.Api     ((@=))
import qualified Web.VK.Api     as VK

main :: IO ()
main = do
    -- Don't forget to create "private" directory with your secrets.
    let token   = $(embedStringFile "private/api-token")
        version = $(embedStringFile "private/api-version")
        me      = $(embedStringFile "private/my-id")
    conn <- VK.mkApiConnDefault token version
    sendMessage "Hello, world!" me conn

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> VK.ApiConn -> IO ()
sendMessage text userId conn = do
    randomId <- VK.randomId
    let method = "messages.send"
        params = ["peer_id" @= userId, "random_id" @= randomId, "message" @= text]
    VK.callMethod_ method params conn
```
