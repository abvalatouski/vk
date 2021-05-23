import           Control.Monad

import           Data.FileEmbed          (embedStringFile)
import           Web.VK.Api.Mtl          ((@=))
import qualified Web.VK.Api.Mtl          as VK
import qualified Web.VK.Api.LongPoll.Mtl as VK

import           ApiMethods
import           Types

main :: IO ()
main = do
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        groupId    = $(embedStringFile "private/group-id")
    runBot bot apiToken apiVersion groupId

bot :: BotM ()
bot = do
    events <- VK.awaitEvents
    forM_ events \case 
        VK.MessageNew VK.Message { .. } -> do
            groupId <- VK.askGroupId
            case messageAction of
                -- Yay! We've just been invited!
                Just (VK.ChatInviteUserById id) | id == -groupId -> do
                    let greeting = "Hello!"
                    sendMessage greeting messagePeerId
                _ ->
                    pure ()
        _ ->
            pure ()
