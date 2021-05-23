# Dealing with VKontakte API

All the examples contain **extremely verbose** passing of API connection from function to function,
which can be fixed with `ReaderT` monad transformer. Also you can use `vk-api-mtl` package which
wraps `vk-api` code inside monad transformers.

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
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        me         = $(embedStringFile "private/my-id")
    conn <- VK.mkApiConnDefault apiToken apiVersion
    sendMessage "Hello, world!" me conn

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> VK.ApiConn -> IO ()
sendMessage text userId conn = do
    randomId <- VK.randomId
    let method = "messages.send"
        params = ["peer_id" @= userId, "random_id" @= randomId, "message" @= text]
    VK.callMethod_ method params conn
```

## Example 2. Message repeater

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad

import           Data.FileEmbed      (embedStringFile)
import           Data.Text           (Text)
import           Web.VK.Api          ((@=))
import qualified Web.VK.Api          as VK
import qualified Web.VK.Api.LongPoll as VK

main :: IO ()
main = do
    -- Don't forget to create "private" directory with your secrets.
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        groupId    = $(embedStringFile "private/group-id")
    conn   <- VK.mkApiConnDefault apiToken apiVersion
    server <- VK.getLongPollServer groupId conn
    forever $ mapM_ (handleEvent conn) =<< VK.awaitEvents conn server

handleEvent :: VK.ApiConn -> VK.Event -> IO ()
handleEvent conn (VK.MessageNew VK.Message { .. }) =
    sendMessage messageText messagePeerId conn
handleEvent _ _ =
    pure ()

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> VK.ApiConn -> IO ()
sendMessage text userId conn = do
    randomId <- VK.randomId
    let method = "messages.send"
        params = ["peer_id" @= userId, "random_id" @= randomId, "message" @= text]
    VK.callMethod_ method params conn

```
