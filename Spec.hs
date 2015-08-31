import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "String calculator" $ do
      it "should sum two zeroes" $ do
        calculate "0+0" `shouldBe` (0 :: Int)


calculate :: String -> Int
calculate _ = 0
