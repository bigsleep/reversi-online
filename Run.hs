{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module Run
( runWai
, run
) where

import Control.Eff (Eff, (:>))
import Control.Eff.Reader.Strict (Reader, runReader)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, runLift)
import qualified Control.Exception
import Data.Typeable (cast)

import Wf.Application.Time (getCurrentTime)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, runLoggerStdIO, LogLevel(..))

import Wf.Network.Wai (toWaiResponse)
import Wf.Network.Http.Response (defaultResponse, file, setStatus)
import qualified Network.Wai as Wai (Request, Response)
import qualified Network.HTTP.Types as HTTP (status500)

import Wf.Session.Stm (Session, SessionSettings, SessionStore, runSessionStm)

type M = Eff
    (  Session
    :> Reader Wai.Request
    :> Logger
    :> Exception
    :> Lift IO
    :> ())

runWai :: SessionStore
    -> SessionSettings
    -> Wai.Request
    -> M Wai.Response
    -> IO Wai.Response
runWai = run handler

    where
    internalError = toWaiResponse . setStatus HTTP.status500 . file "static/error.html" $ defaultResponse ()
    handler (Control.Exception.SomeException e) = do
        print e
        case cast e of
             Just r -> return r
             _ -> return internalError

run :: Control.Exception.Exception e
    => (e -> IO a)
    -> SessionStore
    -> SessionSettings
    -> Wai.Request
    -> M a
    -> IO a
run exceptionHandler sessionStore sessionSettings request a =
    getCurrentTime >>= flip f a

    where
    f t = (>>= handleError)
        . runLift
        . runExc
        . runLoggerStdIO DEBUG
        . flip runReader request
        . runSessionStm sessionStore sessionSettings t
    handleError (Left ex) =
        case Control.Exception.fromException ex of
             Just e -> exceptionHandler e
             _ -> Control.Exception.throw ex
    handleError (Right r) = return r
