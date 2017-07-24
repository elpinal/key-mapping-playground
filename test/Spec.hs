import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "translate" $ do
    it "translates an existing alphabet mapping to a command" $ do
      translate [Alphabet 'j'] `shouldBe` [Accept $ Command "moveDown"]

    it "translates multiple alphabet mappings to commands" $ do
      translate [Alphabet 'j', Alphabet 'k'] `shouldBe` [Accept $ Command "moveDown", Accept $ Command "moveUp"]

    it "does nothing with no existing alphabet mappings" $ do
      translate [Alphabet 'h', Alphabet 'Q', Alphabet 'k'] `shouldBe` [Accept $ Command "moveToLeft", Accept $ Command "moveUp"]

    it "does nothing when given empty list" $ do
      translate [] `shouldBe` []

    it "translates sequential alphabets to a mapped command" $ do
      translate [Alphabet 'h', Alphabet 'i', Alphabet '!'] `shouldBe` [Accept $ Command "hi!"]
