{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module RoomApp
( roomApp
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad (unless)
import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Lift (Lift, lift)
import Wf.Session.Stm (Session, sget)
import qualified Data.List as List (any)
import Data.Array.IArray (Array, (!))
import qualified Network.HTTP.Types as HTTP (status404)

import Wf.Network.Http.Types (ErrorResponse(..))
import Wf.Network.Http.Response (defaultResponse, setStatus)
import Wf.Application.Logger (Logger, logDebug)
import Wf.Application.Exception (Exception, throwException)

import Types
import Login
import Match

roomApp
    :: (Member Session r, Member Exception r, Member Logger r, SetMember Lift (Lift IO) r)
    => Array Int (TVar (MatchRoom Entry))
    -> Eff r (User, Int)
roomApp rooms = do
    user <- sessionUser
    let User uid _ = user
    roomId <- getRoomId
    room <- lift . atomically . readTVar $ rooms ! roomId
    unless (List.any (\(Entry u _ _ _) -> userId u == uid) (matchRoomUsers room)) throwNotFound
    return (user, roomId)
    where
    getRoomId = sget "roomId" >>= maybe throwNotFound return
    throwNotFound = throwException . ErrorResponse . setStatus HTTP.status404 . defaultResponse $ ""
