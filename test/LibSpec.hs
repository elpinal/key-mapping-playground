module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "translate" $ do
    it "translates an existing alphabet mapping to a command" $ do
      translate normalSynonyms [Alphabet 'j'] `shouldBe` [Accept [Name "moveDown"]]

    it "translates multiple alphabet mappings to commands" $ do
      translate normalSynonyms [Alphabet 'j', Alphabet 'k'] `shouldBe` [Accept [Name "moveDown"], Accept [Name "moveUp"]]

    it "does nothing with no existing alphabet mappings" $ do
      translate normalSynonyms [Alphabet 'h', Alphabet 'Q', Alphabet 'k'] `shouldBe` [Accept [Name "moveToLeft"], Accept [Name "moveUp"]]

    it "does nothing when given empty list" $ do
      translate normalSynonyms [] `shouldBe` []

    it "translates sequential alphabets to a mapped command" $ do
      translate normalSynonyms [Alphabet 'h', Alphabet 'i', Alphabet '!'] `shouldBe` [Accept [Name "hi!"]]

    it "switches mode when given a alphabet mapping which is 'Enter x'" $ do
      translate normalSynonyms [Alphabet 'h', Alphabet 'i', Alphabet 'y'] `shouldBe` [Accept [Name "moveToLeft"], Accept [Enter Insert], Accept [Name "input 'y'"]]

    it "returns Pending when given incomplete alphabets" $ do
      translate normalSynonyms [Alphabet 'g'] `shouldBe` [Pending [Alphabet 'g']]

    it "transrates alphabets using modified mappings" $ do
      translateWithMappings normalSynonyms (mapping [Mapping [Alphabet 'h', Alphabet 'i'] [Alphabet 'i', Alphabet 'y', Alphabet '\ESC']])
        [Alphabet 'h', Alphabet 'i', Alphabet 'k'] `shouldBe` [Accept [Enter Insert], Accept [Name "input 'y'"], Accept [Enter Normal], Accept [Name "moveUp"]]

    it "transrates alphabets using modified mappings which include incomplete a command" $ do
      translateWithMappings normalSynonyms (mapping [Mapping [Alphabet 'U', Alphabet 'U'] [Alphabet 'h', Alphabet 'i']])
        [Alphabet 'U', Alphabet 'U', Alphabet '!'] `shouldBe` [Accept [Name "hi!"]]
