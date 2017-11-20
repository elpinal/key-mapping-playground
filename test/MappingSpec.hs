module MappingSpec where

import Test.Hspec

import Control.Arrow
import qualified Data.Map.Lazy as Map

import Mapping

spec :: Spec
spec = do
  describe "lookupF" $
    it "looks up" $ do
      lookupF ['a'] exampleF `shouldBe` (Just 3, [])
      lookupF ['b'] exampleF `shouldBe` (Just 5, [])
      lookupF ['c'] exampleF `shouldBe` (Nothing, ['c'])
      lookupF ['d'] exampleF `shouldBe` (Just 99, [])
      lookupF ['z'] exampleF `shouldBe` (Nothing, ['z'])

      lookupF ['a', 'a'] exampleF `shouldBe` (Just 3, ['a'])
      lookupF ['b', 'z'] exampleF `shouldBe` (Just 5, ['z'])

      lookupF ['c', 'a'] exampleF `shouldBe` (Just 3, [])
      lookupF ['c', 'z'] exampleF `shouldBe` (Nothing, ['c', 'z'])
      lookupF ['c', 'c', 'b'] exampleF `shouldBe` (Just 5, [])

      lookupF ['d', 's'] exampleF `shouldBe` (Just 22, [])
      lookupF ['d', 't'] exampleF `shouldBe` (Just 99, ['t'])
      lookupF ['d', 'z'] exampleF `shouldBe` (Just 99, ['z'])

      lookupF ['d', 't', 'd'] exampleF `shouldBe` (Just 99, [])

  describe "buildCommands" $
    it "builds a map of commands into a recursive function" $ do
      first (fmap (return Normal >>=)) (lookupF [] (buildCommands Map.empty)) `shouldBe` (Nothing, [])
      first (fmap (return Normal >>=)) (lookupF [] (buildCommands $ Map.singleton [NoMod 'a'] $ const $ Just Insert)) `shouldBe` (Nothing, [])
      first (fmap (return Normal >>=)) (lookupF [NoMod 'a'] (buildCommands $ Map.singleton [NoMod 'a'] $ const $ Just Insert)) `shouldBe` (Just (Just Insert), [])
      first (fmap (return Normal >>=)) (lookupF [NoMod 'a', NoMod 'a'] (buildCommands $ Map.singleton [NoMod 'a'] $ const $ Just Insert)) `shouldBe` (Just (Just Insert), [NoMod 'a'])
