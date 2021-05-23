import           Control.Monad
import           Text.Printf

import           Control.Monad.State
import           Data.FileEmbed          (embedStringFile)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text)
import           Lens.Micro
import           Lens.Micro.Mtl
import           Web.VK.Api.Mtl          ((@=))
import qualified Web.VK.Api.Mtl          as VK
import qualified Web.VK.Api.LongPoll.Mtl as VK

import           ApiMethods
import           Commands
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
        VK.MessageNew message@VK.Message { .. } -> do
            conversations <- get
            unless (HashMap.member messagePeerId conversations) do
                let botState = BotState Sleeping
                conversation messagePeerId .= Conversation messagePeerId botState

            groupId <- VK.askGroupId
            case messageAction of
                -- Yay! We've just been invited!
                Just (VK.ChatInviteUserById id) | id == -groupId -> do
                    let greeting =
                           "Hello!\n\
                            \Type /? to find out more about me." :: Text
                    sendMessage messagePeerId greeting
                _ ->
                    pure ()

            currentStatus <- use $ conversation messagePeerId . conversationBotState . botStatus
            case parseSomeCommand messageText of
                Just (Right command) ->
                    runSomeCommand command message
                Just (Left NoSuchCommand) ->
                    sendMessage messagePeerId ("No such command." :: Text)
                Just (Left (CannotParseCommand (SomeCommand proxy))) -> do
                    let error = "Wrong command syntax.\n" <> commandSyntax proxy
                    sendMessage messagePeerId error
                Nothing ->
                    -- This is not command. Ignore the message.
                    pure ()
        _ ->
            pure ()

addConversation :: VK.Id -> BotM ()
addConversation id = do
    let botState = BotState Sleeping
    conversation id .= Conversation id botState
