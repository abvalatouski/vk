import           Control.Monad
import           Text.Printf

import           Control.Monad.State
import           Data.FileEmbed          (embedStringFile)
import qualified Data.HashMap.Strict     as HashMap
import           Lens.Micro
import           Lens.Micro.Mtl
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
bot = forever do
    events <- VK.awaitEvents
    forM_ events \case 
        VK.MessageNew VK.Message { .. } -> do
            conversations <- get
            unless (HashMap.member messagePeerId conversations) do
                let botState = BotState Sleeping
                conversation messagePeerId .= Conversation messagePeerId botState

            groupId <- VK.askGroupId
            case messageAction of
                -- Yay! We've just been invited!
                Just (VK.ChatInviteUserById id) | id == -groupId -> do
                    let greeting = "Hello!"
                    sendMessage greeting messagePeerId
                _ ->
                    pure ()

            currentStatus <- use $ conversation messagePeerId . conversationBotState . botStatus
            case messageText of
                "/on"  ->
                    if currentStatus /= Active then do
                        conversation messagePeerId . conversationBotState . botStatus .= Active
                        sendMessage "Now the bot is active." messagePeerId
                    else
                        sendMessage "The bot is already active." messagePeerId
                "/off" ->
                    when (currentStatus /= Sleeping) do
                        conversation messagePeerId . conversationBotState . botStatus .= Sleeping
                        sendMessage "Now the bot is sleeping." messagePeerId
                _ ->
                    pure ()
        _ ->
            pure ()

addConversation :: VK.Id -> BotM ()
addConversation id = do
    let botState = BotState Sleeping
    conversation id .= Conversation id botState
