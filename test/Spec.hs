import           Data
import           Lib
import           Test.Hspec

main = hspec $ do
    describe "showGrid" $ do
        it "This should concatenate the grid" $ do
            (unlines ["abc", "def", "ghc"]) `shouldBe` "abc\ndef\nghc\n"

    describe "findWord" $ do
        it "Should Find a word in the grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "RUBY" `shouldBe` Just "RUBY"
            findWord grid "PHP" `shouldBe` Just "PHP"
            findWord grid "ELIXIR" `shouldBe` Just "ELIXIR"
        it "Should not find a word that doesn't exist" $ do
            findWord grid "HAPPY" `shouldBe` Nothing

    describe "findWords" $ do 
        it "Should find all the words in the list" $ do
            findWords grid languages `shouldBe` languages
        it "Should not find  words not in the list" $ do
            findWords grid ["APPLES", "HEBREW"] `shouldBe` []

