{-# LANGUAGE OverloadedStrings #-}
module Hakit.Jobs  (
    -- * Get one
    new, Scheduler(),
    -- * Configure
    register, unregister, changeInterval,
    -- * Use
    startExec, stopExec,
    -- * Inform
    isRunning, isRegistered
) where

import qualified Control.Concurrent as C
import qualified Data.Text as T
import qualified Data.IORef as Ref
import qualified Data.Map as M
import qualified Control.Monad as CM
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified Data.Time.Clock.POSIX as P
import qualified Data.Time.Clock as Clock

-- Note: only the job execution, and the jobs themselves run in their own thread, every other functions blocks.

lock :: Scheduler -> IO ()
lock s = MV.putMVar (m s) ()

unlock :: Scheduler -> IO ()
unlock s = CM.void $ MV.tryTakeMVar (m s)

unbox :: Scheduler -> IO Internal
unbox s = Ref.readIORef $ i s

{--------------------------------------------------------------------
  Get one.  
--------------------------------------------------------------------}

data Internal = Internal {
    jobs        :: M.Map T.Text (T.Text, IO ()),    -- | All the jobs registered :: M.Map name (timing, action).
    jobRuns     :: M.Map T.Text P.POSIXTime,        -- | Last time a job ran.
    interval    :: Int,                             -- | Interval in seconds, determines how often does the scheduler look for jobs to execute.
    running     :: Bool,                            -- | True if the scheduler is running.
    threadId    :: Maybe C.ThreadId                 -- | Thread id of the thread running the loop.
}

data Scheduler = Scheduler {
    i           :: Ref.IORef Internal,
    m           :: MV.MVar ()                       -- | Used as a mutex.
}

-- | Gives you a new Scheduler.
-- Default interval is 15 seconds.
new :: IO Scheduler
new = do
    let intern = Internal M.empty M.empty 15 False Nothing
    mvar <- MV.newEmptyMVar
    Ref.newIORef intern >>= \x -> return $ Scheduler x mvar

{--------------------------------------------------------------------
  Configure.  
--------------------------------------------------------------------}

-- | Register a job.
register :: Scheduler -> T.Text -> T.Text -> IO () -> IO ()
register s name timing action = E.bracket_ (lock s) (unlock s) $ do
    Ref.modifyIORef (i s) $ \x -> x{jobs=M.insert name (timing, action) $ jobs x}

-- | Unregisters a job.
unregister :: Scheduler -> T.Text -> IO ()
unregister s name = E.bracket_ (lock s) (unlock s) $ do
    Ref.modifyIORef (i s) $ \x -> x{jobs=M.delete name $ jobs x}

-- | Changes the interval of the job scheduler.
changeInterval :: Scheduler -> Int -> IO ()
changeInterval s interv = E.bracket_ (lock s) (unlock s) $
    Ref.modifyIORef (i s) $ \x -> x{interval = interv}

{--------------------------------------------------------------------
  Use.  
--------------------------------------------------------------------}

recurring :: Pred
recurring form currentTime lastRun
    | is "s"    = due readClean
    | is "m"    = due $ readClean * 60
    | is "h"    = due $ readClean * 3600
    | is "d"    = due $ readClean * 86400
    | is "w"    = due $ readClean * 604800
    where
        is x = T.isSuffixOf x form
        readClean = (fromInteger (read . T.unpack . T.strip $ T.init form))::P.POSIXTime
        due x = currentTime - lastRun > x

type Pred = T.Text -> P.POSIXTime -> P.POSIXTime -> Bool

preds :: [Pred]
preds = [
    recurring
    ]

-- For the time being, this is very primitive.
shallRun :: T.Text -> P.POSIXTime -> P.POSIXTime -> Bool
shallRun form currentTime lastRun = any (\x -> x form currentTime lastRun) preds

execJobs :: Scheduler -> Internal -> IO ()
execJobs s intern = do
    t <- P.getPOSIXTime
    let toRun = filter (\(a, b) -> shallRun (fst b) t $ getLastrun a intern) $ M.toList $ jobs intern
        names = map fst toRun
        actions = map (snd . snd) toRun
        updatedJobRuns = foldl (\a b -> M.insert b t a) (jobRuns intern) names
    E.bracket_ (lock s) (unlock s) $ Ref.modifyIORef (i s) $ \x -> x{jobRuns = updatedJobRuns}
    CM.void $ sequence $ map (\x -> C.forkIO x) actions
    where
    getLastrun :: T.Text -> Internal -> P.POSIXTime
    getLastrun name intern = case M.lookup name (jobRuns intern) of
        Just x  -> x
        Nothing -> 0

-- Interval is only passed from startExec so we avoid another lock when accessing the internals.
loop :: Scheduler -> IO ()
loop s = CM.forever $ do
    intern <- E.bracket_ (lock s) (unlock s) $ unbox s  -- Maybe we are being overly paranoid here with the bracket_
    C.forkIO $ execJobs s intern
    C.threadDelay $ interval intern * 1000000

-- | Start the execution of jobs asyncronously.
-- Jobs can be registered while the execution is running.
-- This function only blocks while it fires up the job execution,
-- then it returns, while the jobs are being executed asyncronously.
-- All jobs are started once the engine is running, then they are scheduled according
-- to their timing.
startExec :: Scheduler -> IO ()
startExec s = E.bracket_ (lock s) (unlock s) $ do
    intern <- unbox s
    if running intern == True
        then return ()
        else do
            tId <- C.forkIO $ loop s
            Ref.modifyIORef (i s) $ \x -> x{running = True, threadId = Just tId}
            return ()

-- | Stop the execution of jobs.
stopExec :: Scheduler -> IO ()
stopExec s = E.bracket_ (lock s) (unlock s) $ do
    intern <- unbox s
    if not $ running intern
        then return () 
        else do
            case threadId intern of
                Just x  -> C.killThread x
                Nothing -> return ()    -- Should always have a thread Id though...
            Ref.modifyIORef (i s) $ \x -> x{running = False, threadId = Nothing}

{--------------------------------------------------------------------
  Inform.  
--------------------------------------------------------------------}

-- | Tells if the jobs are being executed or not.
isRunning :: Scheduler -> IO Bool
isRunning s = E.bracket_ (lock s) (unlock s) $ do
    intern <- unbox s
    return $ running intern

-- | Tells if a job with a given name is already registered.
isRegistered :: Scheduler -> T.Text -> IO Bool
isRegistered s name = E.bracket_ (lock s) (unlock s) $ do
    intern <- unbox s
    return $ M.member name $ jobs intern