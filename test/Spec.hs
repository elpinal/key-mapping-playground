import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "translate" $ do
    it "translates existing alphabet mappings to commands" $ do
      translate [Alphabet 'j'] `shouldBe` [Accept $ Command "moveDown"]
