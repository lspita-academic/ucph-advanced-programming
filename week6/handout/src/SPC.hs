module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg = MsgPing (ReplyChan Int)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcPingCounter :: Int
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure a = SPCM $ pure . \s -> (a, s)
  (<*>) = ap

instance Monad SPCM where
  SPCM m1 >>= f = SPCM $ \s -> do
    (a, s') <- m1 s
    let SPCM m2 = f a in m2 s'

get :: SPCM SPCState
get = SPCM $ \s -> pure (s, s)

put :: SPCState -> SPCM ()
put s = SPCM $ const $ pure ((), s)

io :: IO a -> SPCM a
io m = SPCM $ \s -> do
  a <- m
  pure (a, s)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM s (SPCM m) = do
  (a, _) <- m s
  pure a

-- | A Handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

spcLoop :: Chan SPCMsg -> SPCM ()
spcLoop chan = do
  msg <- io $ receive chan
  handleMsg msg

startSPC :: IO SPC
startSPC = do
  let initialState = SPCState {spcPingCounter = 0}
  server <- spawn $ \c -> runSPCM initialState $ forever $ spcLoop c
  pure $ SPC server

handleMsg :: SPCMsg -> SPCM ()
-- ping
handleMsg (MsgPing chan) = do
  s <- get
  let pong = spcPingCounter s
  put $ s {spcPingCounter = pong + 1}
  io $ reply chan pong

pingSPC :: SPC -> IO Int
pingSPC (SPC server) = requestReply server MsgPing
