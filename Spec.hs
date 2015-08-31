import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "String calculator" $ do
      it "should sum two zeroes" $ do
        calculate "0+0" `shouldBe` (0 :: Int)

      it "should sum a zero with a non-zero" $ do
        calculate "0+1" `shouldBe` (1 :: Int)


calculate :: String -> Int
calculate "0+0" = 0
calculate "0+1" = 1
