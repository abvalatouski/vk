# Dealing with VKontakte API in MTL style

## Example 1. Hello, world!

```haskell
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
```

## Example 2. Message repeater

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad

import           Data.FileEmbed          (embedStringFile)
import           Data.Text               (Text)
import           Web.VK.Api.Mtl          ((@=))
import qualified Web.VK.Api.Mtl          as VK
import qualified Web.VK.Api.LongPoll.Mtl as VK

main :: IO ()
main = do
    -- Don't forget to create "private" directory with your secrets.
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        groupId    = $(embedStringFile "private/group-id")
    VK.runLongPollApi bot groupId =<< VK.mkApiConnDefault apiToken apiVersion

bot :: VK.LongPollM ()
bot = forever $ mapM_ handleEvent =<< VK.awaitEvents

handleEvent :: VK.Event -> VK.LongPollM ()
handleEvent (VK.MessageNew VK.Message { .. }) =
    sendMessage messageText messagePeerId
handleEvent _ =
    pure ()

-- See "https://vk.com/dev/messages.send".
sendMessage :: Text -> VK.Id -> VK.LongPollM ()
sendMessage text userId = do
    randomId <- VK.randomId
    VK.callMethod_ "messages.send"
        [ "peer_id"   @= userId
        , "random_id" @= randomId
        , "message"   @= text
        ]

```
