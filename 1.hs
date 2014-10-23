-- Imports
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf

-- Server Information
server 	= "irc.freenode.org"
port 	= 6667

-- Channel Information
chan 	= "#LadiesStormHackathons"

-- Bot Information
nick 	= "MuseBot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT MuseBot IO
data MuseBot = MuseBot { socket :: Handle }

-- Entry point to the Haskell program
-- Initiates connection to IRC server
main :: IO ()
main = bracket connect disconnect loop
  where
  	disconnect	= hClose . socket
  	loop st 	= runReaderT run st

--
-- Connect to the server and return the initial bot state
--
connect :: IO MuseBot
connect = notify $ do
	h <- connectTo server (PortNumber (fromIntegral port)) -- connect
	hSetBuffering h NoBuffering -- turn off buffering on the socket
	return (MuseBot h)
  where
  	notify a = bracket_
  		(printf "Connecting to %s ... " server >> hFlush stdout)
  		(putStrLn "done.")
  		a

--
-- If we reach this point we've connected successfully.
-- Set nickname, authenticate, and join channel.
-- Start listening for commands to process.
--
run :: Net ()
run = do
	write "NICK" nick
	write "USER" (nick++" 0 * :muse bot")
	write "JOIN" chan
	asks socket >>= listen

--
-- Process each line from the server.
--
listen :: Handle -> Net ()
listen h = forever $ do
	s <- init `fmap` io (hGetLine h)
	io (putStrLn s)
	if ping s then pong s else eval (clean s)
  where
  	forever a 	= a >> forever a
  	clean 		= drop 1 . dropWhile (/= ':') . drop 1 -- Get rid of junk at the beginning
  	ping x 		= "PING :" `isPrefixOf` x -- Ping?
  	pong x 		= write "PONG" (':' : drop 6 x) -- Pong!

--
-- Dispatch a command.
--
eval :: String -> Net ()
eval 		"!quit" 			= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id" `isPrefixOf` x	= privmsg (drop 4 x)
eval   _						= return ()  -- Ignore everything else.


--
-- Send a message out to the server we're currently connected to.
--
write :: String -> String -> Net ()
write s t = do
	h <- asks socket
	io $ hPrintf h 	"%s %s\r\n" s t
	io $ printf 	"> %s %s\n" s t
--
-- Convenience function!
--
io :: IO a -> Net a
io = liftIO

-- Write wrapper
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s) 
