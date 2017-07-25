import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "translate" $ do
    it "translates an existing alphabet mapping to a command" $ do
      translate normalMappings [Alphabet 'j'] `shouldBe` [Accept [Name "moveDown"]]

    it "translates multiple alphabet mappings to commands" $ do
      translate normalMappings [Alphabet 'j', Alphabet 'k'] `shouldBe` [Accept [Name "moveDown"], Accept [Name "moveUp"]]

    it "does nothing with no existing alphabet mappings" $ do
      translate normalMappings [Alphabet 'h', Alphabet 'Q', Alphabet 'k'] `shouldBe` [Accept [Name "moveToLeft"], Accept [Name "moveUp"]]

    it "does nothing when given empty list" $ do
      translate normalMappings [] `shouldBe` []

    it "translates sequential alphabets to a mapped command" $ do
      translate normalMappings [Alphabet 'h', Alphabet 'i', Alphabet '!'] `shouldBe` [Accept [Name "hi!"]]

    it "switches mode when given a alphabet mapping which is 'Enter x'" $ do
      translate normalMappings [Alphabet 'h', Alphabet 'i', Alphabet 'y'] `shouldBe` [Accept [Name "moveToLeft"], Accept [Enter Insert], Accept [Name "input 'y'"]]

    it "returns Pending when given incomplete alphabets" $ do
      translate normalMappings [Alphabet 'g'] `shouldBe` [Pending]
