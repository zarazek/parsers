{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (replicate, length, concat)
import System.IO (Handle, BufferMode(..), hClose, hSetBuffering)
import Network (HostName, PortID(..), withSocketsDo, connectTo) 
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when)
import Control.Exception (Exception, throwIO, finally)
import qualified Data.IntSet as IS
import Text.Parsing.Parser (AttoparsecParser, simpleParse)
import Text.Parsing.LineParser (line)
import Data.ByteString.Char8
import Data.Char (ord)
import System.Environment (getArgs, withArgs)
import Criterion.Main

data DiffType = None | FirstShorter | SecondShorter | DiffAt Int

diffIdx :: ByteString -> ByteString -> DiffType
diffIdx lst1 lst2 = case compare len1 len2 of
                      LT -> FirstShorter
                      GT -> SecondShorter
                      EQ -> go 0 len1
  where len1 = length lst1
        len2 = length lst2
        go idx len | idx == len                       = None
                   | index lst1 idx /= index lst2 idx = DiffAt idx
                   | otherwise                        = go (idx+1) len

bsShow :: Show a => a -> ByteString
bsShow = pack . show

formatDiff :: ByteString -> ByteString -> ByteString
formatDiff expected actual = concat ["Expected '", expected, "', got '", actual, "': ", dsc]
  where dsc = case diffIdx expected actual of
                None          -> "no diff"
                FirstShorter  -> "expected is shorter"
                SecondShorter -> "actual is shorter"
                DiffAt n      -> concat ["diff at ", bsShow n, " character, expected '",
                                         singleton expectedChar, "' (", bsShow expectedCode,
                                         "), actual '",  singleton actualChar, "' (",
                                         bsShow actualCode, ")"]
                                   where expectedChar = index expected  n
                                         expectedCode = ord expectedChar
                                         actualChar = index actual n
                                         actualCode = ord actualChar

data ConversationError = MismatchedExpectation ByteString ByteString
                       | MalformedLine ByteString
                       | InvalidUserId Int
                       | SelfUserId Int
                       | DuplicateUserId Int
             

instance Show ConversationError where
  show (MismatchedExpectation expected actual) = unpack $ formatDiff expected actual
  show (MalformedLine line) = unpack $ concat ["Malformed line '", line, "'"]
  show (InvalidUserId id) = "invalid user ID " ++ show id
  show (SelfUserId id) = "received self user ID " ++ show id
  show (DuplicateUserId id) = "duplicate user ID " ++ show id

instance Exception ConversationError

expect :: ByteString -> Handle -> IO ()
expect str handle = do
  line <- hGetLine handle
  case line == str of
    True  -> return ()
    False -> throwIO (MismatchedExpectation str line)

createName :: Int -> ByteString
createName n = "user" `append` pad 6 '0' (bsShow n)
  where pad n chr str = replicate (n - length str) chr `append` str

while :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
while pred act = go
  where go start = if pred start then return start else act start >>= go

talkToServer :: HostName -> PortID -> Int -> Int -> MVar () -> MVar () -> MVar () -> IO ()
talkToServer address port count myId loggedIn startBanging finished = do
  handle <- connectTo address port
  talk handle `finally` hClose handle
  where talk h = do
          hSetBuffering h LineBuffering
          expect "What's your name?" h
          hPutStrLn h name
          expect (concat ["Welcome to the chat, ", name, "!"]) h
          putMVar loggedIn ()
          takeMVar startBanging
          hPutStrLn h (concat ["hello from ", name, "!"])
          _ <- while containsAll updateSet IS.empty
          putMVar finished ()
          where updateSet set = do
                  line <- hGetLine h
                  id <- parseLine line
                  when (id >= count) (throwIO (InvalidUserId id))
                  when (id == myId) (throwIO (SelfUserId id))
                  when (id `IS.member` set) (throwIO (DuplicateUserId id)) 
                  return (IS.insert id set)
        name = createName myId
        containsAll = (== allIds)
        allIds = IS.fromList ([0..myId-1] ++ [myId+1..count-1])
        parseLine str = maybe onError onSuccess (simpleParse (line :: AttoparsecParser ByteString ByteString Int) str)
          where onError = throwIO (MalformedLine str)
                onSuccess = return
        
data ThreadData = ThreadData { loggedIn_ :: MVar ()
                             , startBanging_ :: MVar ()
                             , finished_ :: MVar ()
                             , threadId_ :: ThreadId }

createThread :: HostName -> PortID -> Int -> Int -> IO ThreadData 
createThread address port count idx = do
  loggedIn <- newEmptyMVar
  startBanging <- newEmptyMVar
  finished <- newEmptyMVar
  threadId <- forkIO (talkToServer address port count idx loggedIn startBanging finished)
  return (ThreadData loggedIn startBanging finished threadId)

pingPong :: HostName -> PortID -> Int -> IO ()
pingPong address port count = do
  threads <- mapM (createThread address port count) [0..count-1]
  mapM_ (takeMVar . loggedIn_) threads
  mapM_ (flip putMVar () . startBanging_) threads
  mapM_ (takeMVar . finished_ ) threads

waitForNextRound = Prelude.putStrLn "sleeping..." >> threadDelay 300000

main :: IO ()
main = withSocketsDo $ do
  (host :  portStr : name : rest) <- getArgs
  let port = PortNumber (fromIntegral (read portStr))
  withArgs rest $ defaultMain [
    bgroup name [ bench "10" $ whnfIO $ pingPong host port 10 >> waitForNextRound
                , bench "100" $ whnfIO $ pingPong host port 100 >> waitForNextRound
                , bench "1000" $ whnfIO  $pingPong host port 1000 >> waitForNextRound ] ]
