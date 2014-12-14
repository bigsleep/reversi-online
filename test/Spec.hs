module Main where

import Match
import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)

import qualified Data.Array.MArray as MArray (getElems)

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

        rs <- atomically $ MArray.getElems rooms

        print rs

        forM_ rs $ \(MatchRoom s us) -> do
            s `shouldBe` Matching
            length us `shouldBe` maxEntry
