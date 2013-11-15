import Data.List
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import KnightsTour

emitByPair :: (a -> a -> c) -> [a] -> [c]
emitByPair f []         = []
emitByPair f (x:y:z:[]) = f x y : f y z : []
emitByPair f (x:y:[])   = f x y : []
emitByPair f (x:[])     = undefined
emitByPair f (x:y:xs)   = f x y : emitByPair f xs

isTour :: Int -> Solution -> Bool
isTour n s                        = allJumpsAreValid && 
                                    hasProperNumOfElements && 
                                    notRepeatedElements
  where allJumpsAreValid          = and $ emitByPair isValidJump s
        isValidJump (r,c) (r',c') = abs(r-r') + abs(c-c') == 3 
        hasProperNumOfElements    = length s == n*n
        notRepeatedElements       = nub s == s

main :: IO ()
main = hspec $ do
  describe "Knight's Tour" $ do

    describe "Auxiliary Functions" $ do

      describe "possibleMovs" $ do

        context "A 3x3 board" $ do

          it "At (1,1) it has just 2 possible movements: (2,3) (3,2)" $ do
            possibleMovs 3 (1,1) `shouldBe` [(2,3),(3,2)]

          it "At (1,3) it has just 2 possible movements: (2,1) (3,2)" $ do
            possibleMovs 3 (1,3) `shouldBe` [(2,1),(3,2)]

          it "At (3,1) it has just 2 possible movements: (2,1) (3,2)" $ do
            possibleMovs 3 (3,1) `shouldBe` [(1,2),(2,3)]

          it "At (3,3) it has just 2 possible movements: (2,1) (3,2)" $ do
            possibleMovs 3 (3,3) `shouldBe` [(1,2),(2,1)]

        context "A 5x5 board" $ do

          it "At the middle (3,3) it has just 8 possible movements" $ do
            possibleMovs 5 (3,3) `shouldBe` [(1,2),(1,4),(2,1),(2,5),(4,1),(4,5),(5,2),(5,4)]

    describe "knightsTour" $ do

      context "A 3x3 board" $ do

        it "Should not have any tour" $ do
          knightsTour 3 `shouldBe` []

      context "A 4x4 board" $ do

        it "Should not have any tour" $ do
          knightsTour 4 `shouldBe` []  

      context "A 5x5 board" $ do

        it "All tours should be valid" $ do
          and (map (isTour 5) (knightsTour 5)) `shouldBe` True  
