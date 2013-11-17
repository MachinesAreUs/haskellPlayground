import Data.List
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import KnightsTour

-- Auxiliary functions

emitByPair :: (a -> a -> c) -> [a] -> [c]
emitByPair f []         = []
emitByPair f (x:y:z:[]) = f x y : f y z : []
emitByPair f (x:y:[])   = f x y : []
emitByPair f (x:y:xs)   = f x y : emitByPair f xs
emitByPair f (x:[])     = undefined

isTour :: Int -> Solution -> Bool
isTour n s                        = allJumpsAreValid && 
                                    hasProperNumOfJumps && 
                                    notRepeatedPositions
  where allJumpsAreValid          = and $ emitByPair isValidJump s
        isValidJump (r,c) (r',c') = abs(r-r') + abs(c-c') == 3 
        hasProperNumOfJumps       = length s == n*n
        notRepeatedPositions      = nub s == s

-- Specs

main :: IO ()
main = hspec $ do
  describe "Knight's Tour" $ do

    describe "possibleMovs" $ do

      context "A 3x3 board" $ do

        it "At (1,1) it has just 2 possible movements: (2,3) (3,2)" $ do
          let expected = [(2,3),(3,2)]
          possibleMovs 3 (1,1) `intersect` expected `shouldBe` expected

        it "At (1,3) it has just 2 possible movements: (2,1) (3,2)" $ do
          let expected = [(2,1),(3,2)]
          possibleMovs 3 (1,3) `intersect` expected `shouldBe` expected

        it "At (3,1) it has just 2 possible movements: (2,1) (2,3)" $ do
          let expected =  [(1,2),(2,3)]
          possibleMovs 3 (3,1) `intersect` expected `shouldBe` expected

        it "At (3,3) it has just 2 possible movements: (1,2) (2,1)" $ do
          let expected = [(1,2),(2,1)]
          possibleMovs 3 (3,3) `intersect` expected `shouldBe` expected

      context "A 5x5 board" $ do

        it "At the center (3,3) it has just 8 possible movements" $ do
          let expected = [(1,2),(1,4),(2,1),(2,5),(4,1),(4,5),(5,2),(5,4)]
          possibleMovs 5 (3,3) `intersect` expected `shouldBe` expected

    describe "knightsTour" $ do

      context "A 3x3 board" $ do

        it "Should have no tour" $ do
          knightsTour 3 `shouldBe` []

      context "A 4x4 board" $ do

        it "Should have no tour" $ do
          knightsTour 4 `shouldBe` []  

      context "A 5x5 board" $ do

        it "All tours should be valid" $ do
          and (map (isTour 5) (knightsTour 5)) `shouldBe` True  

      --context "A 6x6 board" $ do

      --  it "All tours should be valid" $ do
      --    and (map (isTour 6) (knightsTour 6)) `shouldBe` True  


