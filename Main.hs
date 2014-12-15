{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad (when)

import Control.Eff (Member, SetMember, Eff)
import Control.Eff.Lift (Lift, lift)
import qualified Control.Exception

import Wf.Web.Api (apiRoutes, getWai, getApi, postApi)
import Wf.Network.Wai (toWaiResponse)
import Wf.Network.Http.Types (JsonRequest(..), JsonResponse(..))
import Wf.Network.Http.Request (Request(..))
import Wf.Network.Http.Response (defaultResponse, file)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger)
import qualified Wf.Application.Time as T (getCurrentTime)

import qualified Network.Wai as Wai (Application, rawPathInfo, requestHeaders)
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.WebSockets as WS (defaultConnectionOptions)
import qualified Network.Wai.Handler.WebSockets as WaiWS (websocketsOr)

import qualified Data.Array.IArray as A ((!), Array, elems)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.Aeson as DA ((.=), Value, ToJSON(..), object)
import qualified Data.List as List (any)
import Wf.Session.Stm (Session, sget, sput, sttl, sdestroy, initializeSessionStore, defaultSessionSettings)

import Types
import Run
import Match
import Login
import Counter
import Game
import Worker
import RoomApp
import ReversiApp (reversiApp)

main :: IO ()
main = do
    rs <- initializeRooms roomCount
    gs <- initializeGameContexts roomCount
    mapM_ (uncurry startWorker) $ A.elems rs `zip` A.elems gs
    counter <- newCounter 1

    ss <- initializeSessionStore (2 * 60 * 1000000)

    let run' = run throw ss defaultSessionSettings
        runWai' = runWai ss defaultSessionSettings
        throw :: Control.Exception.SomeException -> b
        throw = Control.Exception.throw
        gameApp req pdc = do
            (u, rid) <- run' req (roomApp rs)
            reversiApp (rs A.! rid) (gs A.! rid) u pdc
        server request cont = do
            print (Wai.requestHeaders request)
            apiRoutes staticFileApp routes request cont
        routes =
            [ getApi runWai' "/rooms" $ roomsApp rs
            , postApi runWai' "/login" $ loginApp counter
            , postApi runWai' "/enter" $ enterApp rs
            , getWai "/game" $ \req -> WaiWS.websocketsOr WS.defaultConnectionOptions (gameApp req) notFoundApp $ req
            ]

    Warp.run 3000 server

    where
    roomCount = 10


staticFileApp :: Wai.Application
staticFileApp request = staticFileApp' (B.unpack path) request
    where
    path = Wai.rawPathInfo request


staticFileApp' :: String -> Wai.Application
staticFileApp' fileName _ cont= cont . toWaiResponse $ file ("static/" ++ fileName) $ defaultResponse ()


notFoundApp :: Wai.Application
notFoundApp _ cont = cont . toWaiResponse $ file "static/not_found.html" $ defaultResponse ()


roomsApp :: (Member Session r, SetMember Lift (Lift IO) r) => A.Array Int (TVar (MatchRoom Entry)) -> Request () -> Eff r (JsonResponse DA.Value)
roomsApp rooms _ = do
    u <- maybeSessionUser
    rs <- lift . atomically . mapM readTVar $ A.elems rooms
    return . JsonResponse $ DA.object
        [ "rooms" DA..= DA.toJSON rs
        , "you" DA..= DA.toJSON u
        ]

enterApp
    :: (Member Session r, Member Exception r, Member Logger r, SetMember Lift (Lift IO) r)
    => A.Array Int (TVar (MatchRoom Entry)) -> JsonRequest Int -> Eff r (JsonResponse String)
enterApp rooms (JsonRequest roomId) = do
    user <- sessionUser
    sttl 3600
    let User uid _ = user
    maybeJoined <- sget "roomId" >>= lift . findJoinedRoom uid
    case maybeJoined of
        Just rid | rid == roomId -> return $ JsonResponse "success"
        Just _ -> return $ JsonResponse "failure"
        Nothing -> do
            r <- lift $ do
                now <- T.getCurrentTime
                tryEnter sameUser rooms 2 roomId (Entry user ModePvp False now)
            when r (sput "roomId" roomId)
            return . JsonResponse $ if r then "success" else "failure"

    where
    findJoinedRoom uid (Just rid) = do
        room <- atomically . readTVar $ rooms A.! rid
        if List.any (\(Entry u _ _ _) -> userId u == uid) (matchRoomUsers room)
            then return (Just rid)
            else return Nothing
    findJoinedRoom _ _ = return Nothing

    sameUser (Entry (User uid1 _) _ _ _) (Entry (User uid2 _) _ _ _) = uid1 == uid2
