{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
module Reversi
( Reversi(..)
, ReversiM
, Board
, Player(..)
, Cell(..)
, Move(..)
, GameStatus(..)
, ReversiResult(..)
, ReversiState
, reversiStateStatus
, reversiStateBoard
, reversiStateMoves
, emptyBoard
, initialState
, judge
, move
, enumerateMoves
, reversi
) where

import Data.Typeable (Typeable)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Free (Free(..))
import Control.Monad (when, liftM)
import qualified Data.Array.IArray as Array ((!), (//), Array, listArray, bounds, elems)
import GHC.Generics (Generic)

data Player = Black | White deriving (Show, Eq, Enum, Generic)

data Cell = Empty | Disc Player  deriving (Show, Eq)

type Matrix = Array.Array (Int, Int)

type Board = Matrix Cell

data ReversiResult = BlackWin | WhiteWin | Even deriving (Show, Eq, Enum, Generic)

data Move = Move Player (Int, Int) | NoMove Player deriving (Show, Eq, Generic)

data GameStatus = BlackTurn | WhiteTurn | Ended deriving (Show, Eq, Enum, Generic)

data ReversiState = ReversiState
    { reversiStateStatus :: GameStatus
    , reversiStateBoard :: Board
    , reversiStateMoves :: [Move]
    } deriving (Show, Eq, Generic)

emptyBoard :: Int -> Board
emptyBoard size = Array.listArray ((0, 0), (size - 1, size - 1)) (replicate (size * size) Empty)

initialState :: ReversiState
initialState = ReversiState BlackTurn (emptyBoard 0) []

judge :: Board -> ReversiResult
judge = judge' . foldr f (0, 0) . Array.elems
    where
    judge' (a, b) | a == b = Even
    judge' (a, b) | a > b = BlackWin
    judge' _ = WhiteWin

    f :: Cell -> (Int, Int) -> (Int, Int)
    f Empty bw = bw
    f (Disc Black) (b, w) = (b + 1, w)
    f (Disc White) (b, w) = (b, w + 1)

move :: Move -> Board -> [(Int, Int)]
move (NoMove _) _ = []
move (Move _ q) board
    | outside (Array.bounds board) q
    || board Array.! q /= Empty
        = []

move (Move p (qx, qy)) board =
    concat [ move' (qx + x, qy + y) (x, y) [] | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

    where
    color = Disc p
    move' z _ _
        | outside (Array.bounds board) z
        || board Array.! z == Empty
            = []
    move' z _ zs
        | board Array.! z == color
            = zs
    move' z @ (a, b) (dx, dy) zs = move' (a + dx, b + dy) (dx, dy) (z : zs)

outside :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
outside ((sx, sy), (ex, ey)) (qx, qy) =  qx < sx || ex < qx || qy < sy || ey < qy

enumerateMoves :: Player -> Board -> [(Int, Int)]
enumerateMoves p board = [(x, y) | x <- [sx..ex], y <- [sy..ey], not . null $ move (Move p (x, y)) board]
    where
    ((sx, sy), (ex, ey)) = Array.bounds board

updateBoard :: Cell -> [(Int, Int)] -> Board -> Board
updateBoard c ps board = board Array.// (ps `zip` repeat c)

data Reversi a =
    LoadBoardSize (Int -> a) |
    InputMove Board [(Int, Int)] Player ((Int, Int) -> a) |
    OutputState ReversiState a |
    PutError String
    deriving (Typeable, Functor)

type ReversiM = Free Reversi

loadBoardSize :: StateT ReversiState ReversiM Int
loadBoardSize = lift . Free . LoadBoardSize $ Pure

inputMove :: Board -> [(Int, Int)] -> Player -> StateT ReversiState ReversiM (Int, Int)
inputMove board ms player = lift . Free . InputMove board ms player $ Pure

outputState :: ReversiState -> StateT ReversiState ReversiM ()
outputState s = lift . Free . OutputState s $ Pure ()

putError :: String -> StateT ReversiState ReversiM a
putError = lift . Free . PutError

pushMove :: Move -> StateT ReversiState ReversiM ()
pushMove m = modify $ \s -> s { reversiStateMoves = m : reversiStateMoves s }

getMoves :: StateT ReversiState ReversiM [Move]
getMoves = liftM reversiStateMoves get

getBoard :: StateT ReversiState ReversiM Board
getBoard = liftM reversiStateBoard get

putBoard :: Board -> StateT ReversiState ReversiM ()
putBoard board = modify $ \s -> s { reversiStateBoard = board }

setStatus :: GameStatus -> StateT ReversiState ReversiM ()
setStatus a = modify $ \s -> s { reversiStateStatus = a }

reversi :: ReversiM ReversiResult
reversi = flip evalStateT initialState $ do
    size <- loadBoardSize

    putBoard $ emptyBoard size

    initialMoves $ size `div` 2

    outputState =<< get

    takeWhileM_ (const isNotEnd) $ cycle [turn Black, turn White]

    setStatus Ended

    outputState =<< get

    return . judge =<< getBoard

    where
    initialMoves half =
        putBoard . updateBoard (Disc Black) [(half - 1, half), (half, half - 1)]
                 . updateBoard (Disc White) [(half - 1, half - 1), (half, half)] =<< getBoard

    turn player = do
        board <- getBoard
        let ms = enumerateMoves player board
        if null ms
            then pushMove (NoMove player)
            else do
                setStatus $ if Black == player then BlackTurn else WhiteTurn
                outputState =<< get
                q <- inputMove board ms player
                let ps = move (Move player q) board
                when (null ps) $ putError "impossible move"
                putBoard $ updateBoard (Disc player) (q : ps) board
                pushMove (Move player q)

    isNotEnd = liftM isNotEnd' getMoves

    isNotEnd' (NoMove _ : NoMove _ : _) = False
    isNotEnd' _ = True

    takeWhileM_ _ [] = return ()
    takeWhileM_ a (b : bs) = a b >>= flip when (b >> takeWhileM_ a bs)
