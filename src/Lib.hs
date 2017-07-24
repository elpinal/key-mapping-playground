module Lib
    ( someFunc
    ) where

import qualified Data.Map.Lazy as Map
import Data.Maybe
import Safe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Alphabet = Alphabet Char deriving (Eq, Ord)

newtype Command = Command String deriving (Show)

data Mapping = Mapping [Alphabet] [Alphabet]

type Mappings = Map.Map [Alphabet] [Alphabet]

commandMappings :: Map.Map [Alphabet] Command
commandMappings = Map.fromList
                  [ ([Alphabet 'h'], Command "moveToLeft")
                  , ([Alphabet 'j'], Command "moveDown")
                  , ([Alphabet 'k'], Command "moveUp")
                  , ([Alphabet 'l'], Command "moveToRight")
                  , ([Alphabet 'h', Alphabet '!'], Command "h!")]

data Result = Accept Command
            | Pending
            deriving (Show)

translate :: [Alphabet] -> [Result]
translate [] = []
translate (x:xs) =
  let
    ps = Map.filterWithKey (\(k:ks) _ -> x == k) commandMappings
    ps' = Map.filterWithKey (\(k:ks) _ -> fromMaybe False $ (==) <$> headMay xs <*> headMay ks) ps
  in
    case length ps of
      0 -> translate xs
      1 -> (Accept $ snd (Map.findMin ps)) : translate xs
      otherwise -> if null xs then [result (Map.lookup [x] ps)] else 
                     case length ps' of
                       0 -> maybeToList (Accept <$> Map.lookup [x] ps) ++ translate xs
                       1 -> (Accept $ snd (Map.findMin ps')) : translate (tail xs)
                       otherwise -> undefined

result :: Maybe Command -> Result
result (Just x) = Accept x
result Nothing = Pending
