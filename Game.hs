module Game
( runGame
, initializeGameContexts
, setGameMode
, finalizeGame
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import Control.Monad.Free (Free(..))
import qualified Data.Array.IArray as A (Array, listArray)

import Reversi
import Types


data Input = Input (Int, Int) | Timeout deriving (Show, Eq)

tryInput :: MVar Input -> (Int, Int) -> IO Bool
tryInput mv = tryPutMVar mv . Input

inputDuration :: Int
inputDuration = 30000000

run :: Mode -> TVar GameContext -> Free Reversi ReversiResult -> IO ReversiResult

run _ _ (Pure r) = return r

run m tv (Free (LoadBoardSize f)) = run m tv (f 8)

run m tv (Free (InputMove _ ms p f)) | m == ModePvp || p == Black =
    createInput tv p >>= readMVar >>= handle
    where
    handle (Input s) | s `elem` ms = run m tv (f s)
    handle _ | p == Black = return WhiteWin
    handle _ = return BlackWin

run m tv (Free (InputMove _ ms _ f)) = run m tv . f . head $ ms

run m tv (Free (OutputState s f)) = do
    atomically . modifyTVar' tv $ \x -> x { gameState = s }
    run m tv f

run _ _ (Free (PutError s)) = error s

runGame :: TVar GameContext -> IO ReversiResult
runGame tv = do
    m <- gameMode `fmap` atomically (readTVar tv)
    run m tv reversi

initialGameContext :: GameContext
initialGameContext = GameContext initialState [] ModePvp Nothing emptyInput emptyInput
    where
    emptyInput = const . return $ False

initializeGameContexts :: Int -> IO (A.Array Int (TVar GameContext))
initializeGameContexts n =
    A.listArray (0, n - 1) `fmap` (atomically . mapM newTVar $ replicate n initialGameContext)

setGameMode :: TVar GameContext -> Mode -> IO ()
setGameMode tv m = atomically . modifyTVar' tv $ \x -> x { gameMode = m }

finalizeGame :: TVar GameContext -> IO ()
finalizeGame g = atomically $ writeTVar g initialGameContext

createInput :: TVar GameContext -> Player -> IO (MVar Input)
createInput tv p = do
    mv <- newEmptyMVar
    atomically . modifyTVar' tv $ setInput p mv
    _ <- forkIO $ threadDelay inputDuration >> tryPutMVar mv Timeout >> return ()
    return mv
    where
    setInput Black mv x = x { firstInput = tryInput mv }
    setInput White mv x = x { secondInput = tryInput mv }
