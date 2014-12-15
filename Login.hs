{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Login
( maybeSessionUser
, sessionUser
, loginApp
) where

import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Lift (Lift, lift)
import Wf.Session.Stm (Session, sget, sput, sdestroy, renderSetCookie)
import qualified Data.ByteString as B (ByteString)
import qualified Data.List as List (lookup)
import qualified Network.HTTP.Types as HTTP (status403)

import Wf.Network.Http.Types (ErrorResponse(..))
import Wf.Network.Http.Request (Request(..), UrlEncoded(..))
import Wf.Network.Http.Response (Response, defaultResponse, redirect, addHeader, setStatus)
import Wf.Application.Logger (Logger, logDebug)
import Wf.Application.Exception (Exception, throwException)

import Types
import Counter

sessionUserKey :: B.ByteString
sessionUserKey = "user"

maybeSessionUser :: (Member Session r) => Eff r (Maybe User)
maybeSessionUser = sget sessionUserKey

sessionUser :: (Member Session r, Member Exception r) => Eff r User
sessionUser = sget sessionUserKey >>= handle
    where
    handle Nothing = throwException . ErrorResponse . setStatus HTTP.status403 . defaultResponse $ ""
    handle (Just a) = return a

login :: (Member Session r) => User -> Eff r ()
login = sput sessionUserKey

loginApp :: (Member Session r, Member Logger r, SetMember Lift (Lift IO) r) => Counter -> Request UrlEncoded -> Eff r (Response ())
loginApp counter request =
    case List.lookup "name" params of
         Just (Just name) -> do
            uid <- lift $ countUp counter
            logDebug $ "login name=" ++ show name
            sdestroy
            login (User uid name)
            sc <- renderSetCookie
            return . redirect "/index.html" . addHeader sc $ defaultResponse ()
         _ -> return (defaultResponse ())

    where
    params = unUrlEncoded . requestBody $ request
