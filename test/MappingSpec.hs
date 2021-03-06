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
      let fromNormal = first $ fmap (return Normal >>=)
          fromInsert = first $ fmap (return Insert >>=)
          execFromNormal a = fromNormal . lookupF a
          execFromInsert a = fromInsert . lookupF a
          enterInsert = const $ Just Insert
          enterNormal = const $ Just Normal
          alwaysUndef = const Nothing
          switchNormIns Normal = Just Insert
          switchNormIns Insert = Just Normal

      execFromNormal []                     (buildCommands Map.empty)                               `shouldBe` (Nothing, [])
      execFromNormal []                     (buildCommands $ Map.singleton [NoMod 'a'] enterInsert) `shouldBe` (Nothing, [])
      execFromNormal [NoMod 'a']            (buildCommands $ Map.singleton [NoMod 'a'] enterInsert) `shouldBe` (Just (Just Insert), [])
      execFromNormal [NoMod 'a', NoMod 'a'] (buildCommands $ Map.singleton [NoMod 'a'] enterInsert) `shouldBe` (Just (Just Insert), [NoMod 'a'])
      execFromNormal [NoMod 'a', NoMod 'a'] (buildCommands $ Map.singleton [NoMod 'a'] alwaysUndef) `shouldBe` (Just Nothing, [NoMod 'a'])
      execFromNormal [NoMod 'z']            (buildCommands $ Map.singleton [NoMod 'a'] alwaysUndef) `shouldBe` (Nothing, [NoMod 'z'])

      execFromNormal [NoMod 'a']            (buildCommands $ Map.singleton [NoMod 'a', NoMod 'b'] enterInsert) `shouldBe` (Nothing, [NoMod 'a'])
      execFromNormal [NoMod 'a', NoMod 'b'] (buildCommands $ Map.singleton [NoMod 'a', NoMod 'b'] enterInsert) `shouldBe` (Just (Just Insert), [])

      execFromNormal [NoMod 'a']            (buildCommands $ Map.fromList [([NoMod 'a'], enterNormal), ([NoMod 'a', NoMod 'b'], enterInsert)]) `shouldBe` (Just (Just Normal), [])
      execFromNormal [NoMod 'a', NoMod 'b'] (buildCommands $ Map.fromList [([NoMod 'a'], enterNormal), ([NoMod 'a', NoMod 'b'], enterInsert)]) `shouldBe` (Just (Just Insert), [])

      execFromNormal [NoMod 'a'] (buildCommands $ Map.singleton [NoMod 'a'] switchNormIns) `shouldBe` (Just (Just Insert), [])
      execFromInsert [NoMod 'a'] (buildCommands $ Map.singleton [NoMod 'a'] switchNormIns) `shouldBe` (Just (Just Normal), [])

exampleF :: F Char Int
exampleF = K Nothing f
  where
    f :: Char -> F Char Int
    f 'a' = Z $ Just 3
    f 'b' = Z $ Just 5
    f 'c' = K Nothing f
    f 'd' = K (Just 99) g
    f c = Z Nothing

    g :: Char -> F Char Int
    g 's' = Z $ Just 22
    g 't' = K Nothing f
    g c = Z Nothing
