{-# LANGUAGE OverloadedStrings, DeriveGeneric, ExistentialQuantification #-}
module Types
( User(..)
, WithUser(..)
, GameContext(..)
, Mode(..)
, Entry(..)
) where

import GHC.Generics (Generic)
import qualified Data.ByteString as B (ByteString)
import qualified Data.Aeson as DA (ToJSON(..), FromJSON(..))
import qualified Wf.Application.Time as T (Time)
import Data.Binary (Binary(..))
import Reversi

data User = User
    { userId :: Int
    , userName :: B.ByteString
    } deriving (Show, Eq, Generic)

instance DA.ToJSON User
instance DA.FromJSON User

instance Binary User

data WithUser a = WithUser
    { loginedUser :: Maybe User
    , content :: a
    } deriving (Show, Eq, Generic)

instance DA.ToJSON a => DA.ToJSON (WithUser a)
instance DA.FromJSON a => DA.FromJSON (WithUser a)

data GameContext = GameContext
    { gameState :: ReversiState
    , gameUsers :: [User]
    , gameMode :: Mode
    , gameResult :: Maybe ReversiResult
    , firstInput :: (Int, Int) -> IO Bool
    , secondInput :: (Int, Int) -> IO Bool
    }

data Mode = ModePvp | ModeAi deriving (Show, Eq, Generic)

instance DA.ToJSON Mode
instance DA.FromJSON Mode

data Entry = Entry
    { entryUser :: User
    , entryMode :: Mode
    , entryReady :: Bool
    , entryTime :: T.Time
    } deriving (Show, Generic)

instance DA.ToJSON Entry
instance DA.FromJSON Entry
