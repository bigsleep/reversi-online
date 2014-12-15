module Counter
( Counter
, newCounter
, countUp
) where

import Control.Monad (liftM)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

data Counter = Counter (MVar Int)

newCounter :: Int -> IO Counter
newCounter = liftM Counter . newMVar

countUp :: Counter -> IO Int
countUp (Counter mv) = do
    c <- takeMVar mv
    let n = c + 1
    putMVar mv n
    return n
