{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ReversiApp
( reversiApp
, ReversiApp.Request(..)
, ReversiApp.Response(..)
) where

import Control.Monad (forever, when)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar')
import qualified Network.WebSockets as WS (WebSocketsData(..), PendingConnection, ConnectionException(..), ServerApp, acceptRequest, receiveData, sendTextData, sendBinaryData, sendClose)
import Data.Aeson ((.=))
import qualified Data.Aeson as DA (ToJSON(..), FromJSON(..), encode, decode, object)
import qualified Data.Aeson.Types as DA (Options(..), SumEncoding(..), defaultOptions, genericToJSON, genericParseJSON)
import GHC.Generics (Generic)
import Data.Array.IArray ((!), IArray(bounds))
import qualified Data.ByteString as B (ByteString)
import qualified Data.List as L (any)

import Types
import Reversi
import Match

data Request = Sync Int | Ready | Leave | ChangeMode Mode | Move (Int, Int) deriving (Show, Eq, Generic)

instance DA.ToJSON Request where
    toJSON = DA.genericToJSON options
        where
        options = DA.defaultOptions { DA.sumEncoding = DA.ObjectWithSingleField }

instance DA.FromJSON Request where
    parseJSON = DA.genericParseJSON options
        where
        options = DA.defaultOptions { DA.sumEncoding = DA.ObjectWithSingleField }

data Response = Response
    { responseYou :: User
    , responsePlayer :: Maybe Player
    , responseUsers :: [Entry]
    , responseState :: ReversiState
    , responseResult :: Maybe ReversiResult
    } deriving (Show, Generic)

instance DA.ToJSON Player

instance DA.ToJSON Move

instance DA.ToJSON ReversiResult

instance DA.ToJSON GameStatus

instance DA.ToJSON Response

instance DA.ToJSON ReversiState where
    toJSON rs = DA.object
        [ "status" .= DA.toJSON s
        , "board" .= DA.toJSON board
        , "moves" .= DA.toJSON ms
        ]
        where
        s = reversiStateStatus rs
        b = reversiStateBoard rs
        ms = reversiStateMoves rs
        ((sx, sy), (ex, ey)) = bounds b
        board = [[fromCell $ b ! (x, y) | x <- [sx..ex]] | y <- [sy..ey]]
        fromCell :: Cell -> Int
        fromCell Empty = 0
        fromCell (Disc Black) = 1
        fromCell (Disc White) = 2

reversiApp :: TVar (MatchRoom Entry) -> TVar GameContext -> User -> WS.PendingConnection -> IO ()
reversiApp m g you @ (User uid _) pdc =
    WS.acceptRequest pdc >>= forever . routine
    where
    routine c = WS.receiveData c >>= maybe (return ()) (handle c) . DA.decode

    handle c (Sync mcount) = sendCurrentStateIfChanged c mcount

    handle c Ready = do
        atomically $ modifyTVar' m $ modifyMRUsers (map setReady)
        sendCurrentState c

    handle c Leave = do
        r <- atomically (readTVar m >>= tryKickUser)
        sendCurrentState c
        when r (WS.sendClose c ("leave" :: B.ByteString))
    
    handle c (ChangeMode mode) = do
        atomically $ modifyTVar' m $ modifyMRUsers (map $ setMode mode)
        sendCurrentState c

    handle c (ReversiApp.Move q) =
        atomically (readTVar g) >>= move q >> sendCurrentState c

    modifyMRUsers f mr @ (MatchRoom _ us) = mr { matchRoomUsers = f us }

    setReady e @ (Entry (User uid' _) _ False _) | uid' == uid = e { entryReady = True }

    setReady a = a

    setMode mode e @ (Entry (User uid' _) _ _ _) | uid' == uid = e { entryMode = mode }

    setMode _ a = a

    tryKickUser (MatchRoom True us) | L.any ((== uid) . userId . entryUser) us =
        modifyTVar' m (modifyMRUsers $ filter ((/= uid) . userId . entryUser)) >> return True

    tryKickUser _ = return False

    sendCurrentState c = do
        cur <- getCurrentState
        let result = responseResult $ cur
        WS.sendTextData c . DA.encode $ cur
        when (result /= Nothing) $ WS.sendClose c ("Ended" :: B.ByteString)

    sendCurrentStateIfChanged c mc = do
        cur <- getCurrentState
        let mc' = length . reversiStateMoves . responseState $ cur
            result = responseResult $ cur
        when (mc == 0 || mc /= mc' || result /= Nothing) $ WS.sendTextData c (DA.encode cur)
        when (result /= Nothing) $ WS.sendClose c ("Ended" :: B.ByteString)

    getCurrentState = do
        (room, game) <- atomically $ do
            room <- readTVar m
            game <- readTVar g
            return (room, game)
        let us = matchRoomUsers $ room
            gs = gameState game
            gm = gameMode game
            p = getPlayer gm . gameUsers $ game
            r = gameResult game
        return $ Response you p us gs r

    move q (GameContext _ [_, u1] ModePvp _ chan1 _) | userId u1 == uid = chan1 q

    move q (GameContext _ [u2, _] ModePvp _ _ chan2) | userId u2 == uid = chan2 q

    move q (GameContext _ [u1] ModeAi _ chan1 _) | userId u1 == uid = chan1 q

    move _ _ = return False

    getPlayer ModePvp [_, u1] | userId u1 == uid = Just Black

    getPlayer ModePvp [u2, _] | userId u2 == uid = Just White

    getPlayer ModeAi [u1] | userId u1 == uid = Just Black

    getPlayer _ _ = Nothing
