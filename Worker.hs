module Worker
( startWorker
) where

import Control.Monad (forever, when)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, modifyTVar')
import qualified Wf.Application.Time as T (getCurrentTime, addSeconds)

import Types
import Match
import Game

startWorker :: TVar (MatchRoom Entry) -> TVar GameContext -> IO ThreadId
startWorker room game = forkIO (worker room game)

sleepDuration :: Int
sleepDuration = 10000

matchLimitSeconds :: Integer
matchLimitSeconds = 120

worker :: TVar (MatchRoom Entry) -> TVar GameContext -> IO ()
worker room game =
    forever work
    where
    work = do
        now <- T.getCurrentTime
        let limit = T.addSeconds now (- matchLimitSeconds)
        r <- atomically (readTVar room >>= update limit)
        when r $ do
            putStrLn "started"
            result <- runGame game
            atomically . modifyTVar' game $ \x -> x { gameResult = Just result }
            threadDelay 5000000
            resetRoom room
            finalizeGame game
            return ()
        threadDelay sleepDuration

    update _ (MatchRoom _ es @ [Entry u1 _ True _, Entry u2 _ True _]) =
        writeTVar room (MatchRoom False es) >>
        modifyTVar' game (setGame ModePvp [u1, u2]) >>
        return True

    update _ (MatchRoom _ es @ [Entry u1 ModeAi _ _]) =
        writeTVar room (MatchRoom False es) >>
        modifyTVar' game (setGame ModeAi [u1]) >>
        return True

    update limit (MatchRoom _ es) | length es /= length filtered =
        writeTVar room (MatchRoom True filtered) >>
        return False
        where
        expiredEntry e = entryTime e >= limit
        filtered = filter expiredEntry es

    update _ _ = return False

    setGame mode us g = g { gameMode = mode, gameUsers = us }
