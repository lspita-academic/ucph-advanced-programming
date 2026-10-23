module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
    Job (..),
    JobId,
    jobAdd,
    JobStatus (..),
    JobDoneReason (..),
    jobStatus,
    jobCancel,
    jobWait,
    secondsToMicro,
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
import Data.Foldable (Foldable (fold))
import Data.List (partition)
import Data.Maybe (isJust)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

secondsToMicro :: Int -> Int
secondsToMicro s = s * 1000000

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg
  = MsgPing (ReplyChan Int)
  | MsgJobAdd Job (ReplyChan JobId)
  | MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | MsgJobCancel JobId
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

instance Enum JobId where
  toEnum = JobId
  fromEnum (JobId i) = i

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)

type JobHandle = (JobId, Job)

type JobDoneHandle = (JobId, JobDoneReason)

type JobDoneListenerHandle = (JobId, ReplyChan (Maybe JobDoneReason))

data SPCState = SPCState
  { spcPingCounter :: Int,
    nextJobId :: JobId,
    scpPendingJobs :: [JobHandle],
    scpDoneJobs :: [JobDoneHandle],
    scpListeners :: [JobDoneListenerHandle]
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

-- | A Handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

scpDefaultState :: SPCState
scpDefaultState =
  SPCState
    { spcPingCounter = 0,
      nextJobId = toEnum 0,
      scpPendingJobs = [],
      scpDoneJobs = [],
      scpListeners = []
    }

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

completeJob :: JobId -> JobDoneReason -> SPCM ()
completeJob jobId reason = do
  state <- get
  let pending' = removeAssoc jobId $ scpPendingJobs state
  let done' = (jobId, reason) : scpDoneJobs state
  let (matchingListeners, listeners') = partition ((== jobId) . fst) $ scpListeners state
  forM_ matchingListeners $ \(_, chan) -> io $ reply chan $ Just reason
  put $
    state
      { scpPendingJobs = pending',
        scpDoneJobs = done',
        scpListeners = listeners'
      }

pingSPC :: SPC -> IO Int
pingSPC (SPC server) = requestReply server MsgPing

jobsListLookup :: JobId -> [(JobId, b)] -> Maybe b
jobsListLookup = lookup

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC server) job = requestReply server $ MsgJobAdd job

jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC server) jobId = requestReply server $ MsgJobStatus jobId

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC server) jobId = sendTo server $ MsgJobCancel jobId

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC server) jobid = requestReply server $ MsgJobWait jobid

spcLoop :: Chan SPCMsg -> SPCM ()
spcLoop chan = do
  msg <- io $ receive chan
  handleMsg msg

startSPC :: IO SPC
startSPC = do
  server <- spawn $ \c -> runSPCM scpDefaultState $ forever $ spcLoop c
  pure $ SPC server

handleMsg :: SPCMsg -> SPCM ()
handleMsg (MsgPing chan) = do
  state <- get
  let pong = spcPingCounter state
  put $ state {spcPingCounter = pong + 1}
  io $ reply chan pong
handleMsg (MsgJobAdd job chan) = do
  state <- get
  let pendingJobs = scpPendingJobs state
  let jobId = nextJobId state
  put $ state {nextJobId = succ jobId, scpPendingJobs = (jobId, job) : pendingJobs}
  io $ reply chan jobId
handleMsg (MsgJobStatus jobId chan) = do
  state <- get
  let searchJob f = jobsListLookup jobId $ f state
  let status = case (searchJob scpPendingJobs, searchJob scpDoneJobs) of
        (Just _, _) -> Just JobPending
        (_, Just reason) -> Just $ JobDone reason
        _ -> Nothing
  io $ reply chan status
handleMsg (MsgJobCancel jobId) = do
  state <- get
  let pending = scpPendingJobs state
  case jobsListLookup jobId pending of
    Nothing -> pure ()
    Just _ -> completeJob jobId DoneCancelled
handleMsg (MsgJobWait jobId chan) = do
  state <- get
  let searchJob f = jobsListLookup jobId $ f state
  case (searchJob scpDoneJobs, searchJob scpPendingJobs) of
    (Just reason, _) -> io $ reply chan $ Just reason
    (_, Just _) ->
      let listeners' = (jobId, chan) : scpListeners state
       in put $ state {scpListeners = listeners'}
    _ -> io $ reply chan Nothing
