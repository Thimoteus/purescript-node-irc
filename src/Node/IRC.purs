module Node.IRC
  ( IRC()
  , Setup()
  , Host(..), runHost
  , Channel(..), runChannel
  , MessageText(..), runMessageText
  , Nick(..), runNick
  , connect
  , sayChannel
  , sayNick
  , WhoIs (..)
  , whoIs
  , runWhoIs
  , ChannelMessageEvent()
  , onChannelMessage
  , PrivateMessageEvent()
  , onPrivateMessage
  , JoinEvent ()
  , onJoinEvent
  , PartEvent ()
  , onPartEvent
  ) where

import Prelude
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Undefined
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Console (log, print, CONSOLE())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Aff
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)
import qualified Data.Array.Unsafe as AU
import Unsafe.Coerce (unsafeCoerce)

import qualified Node.IRC.BareBones as BareBones

-------------
-- Re-exports

type IRC = BareBones.IRC

-----------
-- The rest

type Setup e a = ReaderT BareBones.Client (Aff (irc :: IRC, console :: CONSOLE | e)) a

runSetup :: forall e a.
  Setup e Unit -> BareBones.Client -> Eff (irc :: IRC, console :: CONSOLE | e) Unit
runSetup setup client =
  runAff print return (runReaderT setup client)

newtype Host = Host String

runHost :: Host -> String
runHost (Host s) = s

newtype Channel = Channel String

runChannel :: Channel -> String
runChannel (Channel s) = s

newtype MessageText = MessageText String

runMessageText :: MessageText -> String
runMessageText (MessageText s) = s

newtype Nick = Nick String

instance eqNick :: Eq Nick where
  eq (Nick n) (Nick m) = n == m

instance showNick :: Show Nick where
  show (Nick n) = "Nick " <> n

runNick :: Nick -> String
runNick (Nick s) = s

connect :: forall e.
  Host -> Nick -> Channel -> Setup e Unit -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
connect (Host host) (Nick nick) chan setup = do
  client <- liftEff $ do
    c <- BareBones.createClient host nick [runChannel chan]
    -- Add an error handler, because otherwise errors will crash the whole
    -- program
    BareBones.addListener c "error"
      { fromArgumentsJS: unsafeFirstArgument, action: printInspect }
    return c

  waitForEvent client "registered"
  waitForEvent client "join"

  -- Set it up
  runReaderT setup client

  where
  waitForEvent client eventType =
    makeAff \_ success ->
      BareBones.once client eventType
        { fromArgumentsJS: const unit, action: success }

sayChannel :: forall e.
  Channel -> MessageText -> Setup e Unit
sayChannel (Channel chan) (MessageText text) =
  ReaderT \client -> liftEff $ BareBones.say client chan text

sayNick :: forall e.
  BareBones.Client -> Nick -> MessageText -> Eff (irc :: IRC | e) Unit
sayNick client (Nick nick) (MessageText text) =
  BareBones.say client nick text

whoIs :: forall e. Nick -> (F WhoIs -> Setup e Unit) -> Setup e Unit
whoIs (Nick nick) cb = ReaderT \ client ->
                    liftEff $ BareBones.whois client nick
                      { fromArgumentsJS: read <<< foreignFromArgumentsJS <<< unsafeNthArgument 0
                      , action: \ info -> runSetup (cb info) client }

foreignFromArgumentsJS :: BareBones.ArgumentsJS -> Foreign
foreignFromArgumentsJS = toForeign

newtype WhoIs = WhoIs { nick :: Nick
                      , user :: Maybe String
                      , host :: Maybe String
                      , realname :: Maybe String
                      , channels :: Maybe (Array Channel)
                      , server :: Maybe String
                      , serverinfo :: Maybe String
                      , account :: Maybe String
                      , accountinfo :: Maybe String }

instance showWhoIs :: Show WhoIs where
  show (WhoIs w) = "WhoIs " <> show w.nick

runWhoIs :: WhoIs -> _
runWhoIs (WhoIs w) = w

instance nickIsForeign :: IsForeign Nick where
  read value = Nick <$> readString value

instance channelIsForeign :: IsForeign Channel where
  read value = Channel <$> readString value

instance whoIsForeign :: IsForeign WhoIs where
  read value = do
    nick <- readProp "nick" value
    user <- readUndefined (readProp "user") value
    host <- readUndefined (readProp "host") value
    realname <- readUndefined (readProp "realname") value
    channels <- readUndefined (readProp "channels") value
    server <- readUndefined (readProp "server") value
    serverinfo <- readUndefined (readProp "serverinfo") value
    account <- readUndefined (readProp "account") value
    accountinfo <- readUndefined (readProp "accountinfo") value
    return $ WhoIs { nick: nick
                   , user: runUndefined user
                   , host: runUndefined host
                   , realname: runUndefined realname
                   , channels: runUndefined channels
                   , server: runUndefined server
                   , serverinfo: runUndefined serverinfo
                   , account: runUndefined account
                   , accountinfo: runUndefined accountinfo }

type ChannelMessageEvent =
  { nick :: Nick
  , text :: MessageText
  }

type PrivateMessageEvent = { nick :: Nick
                           , to :: Nick
                           , text :: MessageText }

type JoinEvent = ChannelMessageEvent 
type PartEvent = ChannelMessageEvent

onPrivateMessage :: forall e. (PrivateMessageEvent -> Setup e Unit) -> Setup e Unit
onPrivateMessage cb =
  ReaderT \ client ->
    liftEff $ BareBones.addListener client "pm"
      { fromArgumentsJS: privateMessageFromArgumentsJS
      , action: \ event -> runSetup (cb event) client }

privateMessageFromArgumentsJS :: BareBones.ArgumentsJS -> PrivateMessageEvent
privateMessageFromArgumentsJS args = { nick: unsafeNthArgument 0 args
                                     , to: unsafeNthArgument 1 args
                                     , text: unsafeNthArgument 2 args }

onJoinEvent :: forall e. Channel -> (JoinEvent -> Setup e Unit) -> Setup e Unit
onJoinEvent chan cb =
  ReaderT \ client ->
    liftEff $ BareBones.addListener client ("join" <> runChannel chan)
      { fromArgumentsJS: joinFromArgumentsJS
      , action: \ event -> runSetup (cb event) client }

joinFromArgumentsJS :: BareBones.ArgumentsJS -> JoinEvent
joinFromArgumentsJS = channelMessageFromArgumentsJS

onPartEvent :: forall e. Channel -> (PartEvent -> Setup e Unit) -> Setup e Unit
onPartEvent chan cb =
  ReaderT \ client ->
    liftEff $ BareBones.addListener client ("part" <> runChannel chan)
      { fromArgumentsJS: partFromArgumentsJS
      , action: \ event -> runSetup (cb event) client }

partFromArgumentsJS :: BareBones.ArgumentsJS -> PartEvent
partFromArgumentsJS = channelMessageFromArgumentsJS

-- | Add a callback to be run every time a message is sent to a particular
-- | channel.
onChannelMessage :: forall e.
  Channel
  -> (ChannelMessageEvent -> Setup e Unit)
  -> Setup e Unit
onChannelMessage chan cb =
  ReaderT \client -> liftEff $
    BareBones.addListener client
                          (toStr chan)
                          (mkCallback client)
  where
  toStr c =
    "message" <> runChannel c
  mkCallback client =
    { fromArgumentsJS: channelMessageFromArgumentsJS
    , action: \event -> runSetup (cb event) client
    }

channelMessageFromArgumentsJS :: BareBones.ArgumentsJS -> ChannelMessageEvent
channelMessageFromArgumentsJS args = { nick: unsafeNthArgument 0 args
                                     , text: unsafeNthArgument 1 args }

unsafeNthArgument :: forall a. Int -> BareBones.ArgumentsJS -> a
unsafeNthArgument n = flip AU.unsafeIndex n <<< unsafeCoerce

unsafeFirstArgument :: forall a. BareBones.ArgumentsJS -> a
unsafeFirstArgument = unsafeNthArgument 1

printInspect :: forall e a. a -> Eff (console :: CONSOLE | e) Unit
printInspect = log <<< inspect

foreign import inspect :: forall a. a -> String
