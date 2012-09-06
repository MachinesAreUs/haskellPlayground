module ShapesAreaSpec (main, spec) where
  
import Test.Hspec
import qualified Test.Hspec.Core as H
import Shapes


main = hspec spec

spec = do
    describe "area" $ do
      it "calculates the area of a simple rectangle" $
        area (Rectangle 5 5)  == 25
      
      it "calculates the area of a simple circle" $
        area (Ellipse 1 1) == 3.1416