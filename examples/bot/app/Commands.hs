module Commands
  where

import           Control.Monad
import           Data.Char
import           Data.Functor.Identity
import           Data.List
import           Data.Proxy

import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.Attoparsec.Text    as Parse
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text)
import qualified Data.Text.Lazy.Builder  as Text
import           Lens.Micro.Mtl
import qualified Web.VK.Api.LongPoll.Mtl as VK

import           ApiMethods
import           Types

class Command a where
    runCommand    :: a -> VK.Message -> BotM ()
    commandName   :: Proxy a -> Text
    payloadParser :: Proxy a -> Parser a
    commandDesc   :: Proxy a -> Text.Builder
    commandSyntax :: Proxy a -> Text.Builder

commandHelp :: Command a => Proxy a -> Text.Builder
commandHelp proxy =
    let desc   = commandDesc proxy
        syntax = commandSyntax proxy
     in syntax <> "\n\n" <> desc

-- Helps to use dynamic dispatch.
data SomeCommand f where
    SomeCommand :: forall a f. Command a => f a -> SomeCommand f

runSomeCommand :: SomeCommand Identity -> VK.Message -> BotM ()
runSomeCommand (SomeCommand (Identity command)) = runCommand command

data ParseCommandError
    = NoSuchCommand
    | CannotParseCommand (SomeCommand Proxy)

parseCommand ::
    HashMap Text (SomeCommand Proxy)
 -> Text
 -> Maybe (Either ParseCommandError (SomeCommand Identity))
parseCommand proxies = parse do
    void $ Parse.char '/'
    name <- Parse.takeTill isSpace
    Parse.skipSpace
    case HashMap.lookup name proxies of
        Just (SomeCommand proxy) -> do
            command <- parse (payloadParser proxy) <$> Parse.takeText
            case command of
                Just command ->
                    pure $ Right $ SomeCommand $ Identity command
                Nothing ->
                    pure $ Left $ CannotParseCommand (SomeCommand proxy)
        Nothing ->
            pure $ Left NoSuchCommand
  where
    parse parser =
        either (const Nothing) Just
      . parseOnly (parser <* Parse.endOfInput)

parseSomeCommand :: Text -> Maybe (Either ParseCommandError (SomeCommand Identity))
parseSomeCommand = parseCommand commandProxiesTable

-- Commands.

commandProxies :: [SomeCommand Proxy]
commandProxies =
    [ SomeCommand $ Proxy @Help
    , SomeCommand $ Proxy @On
    , SomeCommand $ Proxy @Off
    , SomeCommand $ Proxy @Print
    ]

commandProxiesTable :: HashMap Text (SomeCommand Proxy)
commandProxiesTable =
    HashMap.fromList
  $ fmap (\(SomeCommand proxy) -> (commandName proxy, SomeCommand proxy)) commandProxies

-- Prints the given message.
newtype Print = Print Text

instance Command Print where
    runCommand (Print message) VK.Message { .. } = do
        status <- use $ conversation messagePeerId . conversationBotState . botStatus
        when (status == Active) do
            sendMessage messagePeerId message
    
    commandName = const
        "print"

    payloadParser = const do
        message <- Parse.takeText
        pure $ Print message

    commandDesc = const
        "Prints the given message."

    commandSyntax = const
        "/print\nSOME TEXT"

-- Wakes up the bot.
data On = On

instance Command On where
    runCommand On VK.Message { .. } = do
        status <- use $ conversation messagePeerId . conversationBotState . botStatus
        if status == Active then
            sendMessage messagePeerId ("The bot is already active." :: Text)
        else do
            conversation messagePeerId . conversationBotState . botStatus .= Active
            sendMessage messagePeerId ("Now the bot is active." :: Text)
    
    commandName = const
        "on"

    payloadParser = const do
        pure On

    commandDesc = const
        "Turns on the bot.\n\
        \After execution of that command the bot will respond to incoming commands.\n\
        \The default state of the bot is OFF."
    
    commandSyntax = const
        "/on"

-- Turns off the bot.
data Off = Off

instance Command Off where
    runCommand Off VK.Message { .. } = do
        status <- use $ conversation messagePeerId . conversationBotState . botStatus
        when (status == Active) do
            sendMessage messagePeerId ("Now the bot is sleeping." :: Text)
            conversation messagePeerId . conversationBotState . botStatus .= Sleeping
    
    commandName = const
        "off"

    payloadParser = const do
        pure Off
    
    commandDesc = const
        "Turns off the bot.\n\
        \After execution of that command the bot will respond only to /? and /on commands.\n\
        \The default state of the bot is OFF."
    
    commandSyntax = const
        "/off"

-- Show the help message.
data Help = Help

instance Command Help where
    runCommand Help VK.Message { .. } = do
        let help =
                mconcat
              $ intersperse "\n\n##########\n\n"
              $ ("LIST OF COMMANDS" :)
              $ fmap (\(SomeCommand proxy) -> commandHelp proxy)
                commandProxies
        sendMessage messagePeerId help

    commandName = const
        "?"

    payloadParser = const do
        pure Help
    
    commandDesc = const
        "Shows this help message."
    
    commandSyntax = const
        "/?"
