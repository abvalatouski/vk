{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad

import           Data.FileEmbed      (embedStringFile)
import           Data.Text           (Text)
import qualified Data.Text.IO        as Text
import           Web.VK.Api          ((@=))
import qualified Web.VK.Api          as VK
import qualified Web.VK.Api.LongPoll as VK

main :: IO ()
main = do
    -- Don't forget to create "private" directory with your secrets.
    let token   = $(embedStringFile "private/api-token")
        version = $(embedStringFile "private/api-version")
        groupId = $(embedStringFile "private/group-id")
    conn   <- VK.mkApiConnDefault token version
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