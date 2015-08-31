import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "String calculator" $ do
      describe "should sum:" $ do
        it "two zeroes" $ do
          calculate "0+0" `shouldBe` (0 :: Int)
  
        it "a zero with a non-zero" $ do
          calculate "0+1" `shouldBe` (1 :: Int)
  
        it "a non-zero with something" $ do
          calculate "1+1" `shouldBe` (2 :: Int)
  
      describe "should subtract:" $ do
        it "a zero with a non-zero" $ do
          calculate "0-1" `shouldBe` (-1 :: Int)
  
        it "and be a positive number" $ do
          calculate "9-1" `shouldBe` (8 :: Int)
  
  
calculate :: String -> Int
calculate expression = first_operand `operator` second_operand
    where parts' = T.chunksOf 1 $ T.pack expression
          parts = (map T.unpack parts')
          first_operand = read (parts !! 0) 
          second_operand = read (parts !! 2) 
	  operator = decideOperator $ parts !! 1


decideOperator part = case part of
    "+" -> (+)
    "-" -> (-)
