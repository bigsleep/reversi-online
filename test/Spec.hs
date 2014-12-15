module Main where

import Match
import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)

import qualified Data.Array.IArray as A (elems)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

main :: IO ()
main = hspec $ do
    tryMatchSpec


tryMatchSpec :: Spec
tryMatchSpec = describe "tryMatch" $ do
    it "keep user count smaller than or equal to maxEntry" $ do
        let maxEntry = 4
        let roomCount = 10
        let threadCount = maxEntry * roomCount

        rooms <- initializeRooms roomCount

        forM_ [1..threadCount] $ \i -> forkIO $ do
            tryMatch rooms maxEntry i
            return ()

        threadDelay 100000

        rs <- mapM (atomically . readTVar) $ A.elems rooms

        print rs

        forM_ rs $ \(MatchRoom s us) -> do
            s `shouldBe` True
            length us `shouldBe` maxEntry
