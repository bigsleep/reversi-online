{-# LANGUAGE DeriveGeneric #-}
module Match
( MatchRoom(..)
, initializeRooms
, resetRoom
, tryMatch
, tryEnter
, tryExit
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import qualified Data.Array.IArray as A ((!), Array, IArray(..), listArray)
import qualified Data.List as List (delete)
import GHC.Generics (Generic)
import qualified Data.Aeson as DA (ToJSON(..), FromJSON(..))

data MatchRoom user = MatchRoom
    { matchRoomActive :: Bool
    , matchRoomUsers :: [user]
    } deriving (Show, Eq, Generic)

instance DA.ToJSON user => DA.ToJSON (MatchRoom user)
instance DA.FromJSON user => DA.FromJSON (MatchRoom user)


initializeRooms :: Int -> IO (A.Array Int (TVar (MatchRoom a)))
initializeRooms roomCount =
    A.listArray (0, roomCount - 1) `fmap` (atomically . mapM newTVar $ replicate roomCount empty)
    where
    empty = MatchRoom True []

tryMatch :: (Eq a) => A.Array Int (TVar (MatchRoom a)) -> Int -> a -> IO (Maybe Int)
tryMatch rooms maxEntry user = loop $ A.bounds rooms
    where
    loop (i, e) | e < i = return Nothing
    loop (i, e) = do
        r <- atomically $ match i
        if r then return $ Just i
             else loop (i + 1, e)

    match i = do
        let tv = rooms A.! i
        room <- readTVar tv
        if canEnter room
            then writeTVar tv (addUser room user) >> return True
            else return False
        where
        canEnter (MatchRoom True us) | length us < maxEntry && user `notElem` us = True
        canEnter _ = False

tryEnter :: (a -> a -> Bool) -> A.Array Int (TVar (MatchRoom a)) -> Int -> Int -> a -> IO Bool
tryEnter sameEntry rooms maxEntry roomId entry = atomically . enterIfExists $ A.bounds rooms
    where
    enterIfExists (i, e) | roomId < i || e < roomId = return False

    enterIfExists _ = do
        let tv = rooms A.! roomId
        room <- readTVar tv
        enter tv room

    enter _ (MatchRoom True es) | any (sameEntry entry) es = return True

    enter tv room @ (MatchRoom True es) | length es < maxEntry =
        writeTVar tv (addUser room entry) >> return True

    enter _ _ = return False

tryExit :: (Eq a) => A.Array Int (TVar (MatchRoom a)) -> Int -> a -> IO Bool
tryExit rooms roomId user = atomically . exitIfExists $ A.bounds rooms
    where
    exitIfExists (i, e) | roomId < i || e < roomId = return False
    exitIfExists _ = do
        let tv = rooms A.! roomId
        room <- readTVar tv
        if canExit room
            then writeTVar tv (removeUser room user) >> return True
            else return False

    canExit (MatchRoom True us) | user `elem` us = True
    canExit _ = False

addUser :: MatchRoom user -> user -> MatchRoom user
addUser (MatchRoom s us) user = MatchRoom s (user : us)

removeUser :: (Eq user) => MatchRoom user -> user -> MatchRoom user
removeUser (MatchRoom s us) user = MatchRoom s (List.delete user us)

resetRoom :: TVar (MatchRoom user) -> IO ()
resetRoom room = atomically . writeTVar room $ MatchRoom True []
