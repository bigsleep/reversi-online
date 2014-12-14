module Match
( MatchState(..)
, MatchRoom(..)
, initializeRooms
, tryMatch
, tryEnter
, tryExit
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TArray (TArray)
import qualified Data.Array.MArray as Array (MArray(getBounds, newArray), readArray, writeArray)
import qualified Data.List as List (deleteBy)

data MatchState = Matching | Started | Ended deriving (Show, Eq)

data MatchRoom user = MatchRoom
    { matchRoomState :: MatchState
    , matchRoomUsers :: [(user, Bool)]
    } deriving (Show, Eq)

initializeRooms :: Int -> IO (TArray Int (MatchRoom a))
initializeRooms roomCount = atomically $ Array.newArray (0, roomCount - 1) empty
    where
    empty = MatchRoom Matching []

tryMatch :: (Eq a) => TArray Int (MatchRoom a) -> Int -> a -> IO (Maybe Int)
tryMatch rooms maxEntry user = atomically match
    where
    match = Array.getBounds rooms >>= loop

    loop (i, e) | e < i = return Nothing
    loop (i, e) = Array.readArray rooms i >>= enter
        where
        enter room @ (MatchRoom Matching us) | length us < maxEntry && user `notElem` map fst us =
            Array.writeArray rooms i (addUser room user) >> return (Just i)
        enter _ = loop (i + 1, e)

tryEnter :: (Eq a) => TArray Int (MatchRoom a) -> Int -> Int -> a -> IO Bool
tryEnter rooms maxEntry roomId user = atomically $ Array.getBounds rooms >>= enterIfExists
    where
    enterIfExists (i, e) | roomId < i || e < roomId = return False

    enterIfExists _ = Array.readArray rooms roomId  >>= enter

    enter (MatchRoom Matching us) | user `notElem` map fst us = return True

    enter room @ (MatchRoom Matching us) | length us < maxEntry =
        Array.writeArray rooms roomId (addUser room user) >> return True

    enter _ = return False

tryExit :: (Eq a) => TArray Int (MatchRoom a) -> Int -> a -> IO Bool
tryExit rooms roomId user = atomically $ Array.getBounds rooms >>= exitIfExists
    where
    exitIfExists (i, e) | roomId < i || e < roomId = return False
    exitIfExists _ = Array.readArray rooms roomId >>= exit
    exit room @ (MatchRoom Matching us) | user `elem` map fst us =
        Array.writeArray rooms roomId (removeUser room user) >> return True
    exit _ = return False

addUser :: MatchRoom user -> user -> MatchRoom user
addUser (MatchRoom s us) user = MatchRoom s ((user, False) : us)

removeUser :: (Eq user) => MatchRoom user -> user -> MatchRoom user
removeUser (MatchRoom s us) user = MatchRoom s (List.deleteBy sameUser (user, False) us)
    where
    sameUser (ua, _) (ub, _) = ua == ub
