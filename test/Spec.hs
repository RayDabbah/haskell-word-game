import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do 
    describe "This is how to do a test" $ do
        it "Should Run tests!" $ do
            someString `shouldBe` "someSuperFunc"
