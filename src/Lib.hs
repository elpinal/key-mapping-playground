module Lib where

import qualified Data.Map.Lazy as Map
import Data.Maybe
import Safe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Alphabet = Alphabet Char deriving (Show, Eq, Ord)

newtype Command = Command String deriving (Show, Eq)

data Mapping = Mapping [Alphabet] [Alphabet]

type Mappings = Map.Map [Alphabet] [Alphabet]

-- Note: Don't use empty list for keys.
commandMappings :: Map.Map [Alphabet] Command
commandMappings = Map.fromList
                  [ ([Alphabet 'h'], Command "moveToLeft")
                  , ([Alphabet 'j'], Command "moveDown")
                  , ([Alphabet 'k'], Command "moveUp")
                  , ([Alphabet 'l'], Command "moveToRight")
                  , ([Alphabet 'h', Alphabet '!'], Command "h!")
                  , ([Alphabet 'h', Alphabet 'i', Alphabet '!'], Command "hi!")]

data Result = Accept Command
            | Pending
            deriving (Show, Eq)

translate :: [Alphabet] -> [Result]
translate [] = []
translate xs = translateN commandMappings xs 0

translateN :: Map.Map [Alphabet] Command -> [Alphabet] -> Int -> [Result]
translateN ps xs n =
  let
    xs' = drop n xs
    ps' = Map.filterWithKey (\ks _ -> fromMaybe False $ (==) <$> headMay xs' <*> ks `atMay` n) ps

    c0 = Accept <$> Map.lookup (take n xs) ps
    c = Map.lookup (take (n+1) xs) ps'
  in
    case length ps' of
      0 -> backtrack c0 xs n commandMappings
      1 | isJust c -> result c : translate (tail xs')
      _ -> if length xs == n+1 then [result c] else translateN ps' xs (n+1)

backtrack :: Maybe Result -> [Alphabet] -> Int -> Map.Map [Alphabet] Command -> [Result]
backtrack c0 xs n original = if isJust c0 then fromJust c0 : translate (drop n xs) else
  let
    c1 = Accept <$> Map.lookup (take (n-1) xs) original
  in
    if n == 0 then translate (tail xs) else backtrack c1 xs (n-1) original

result :: Maybe Command -> Result
result (Just x) = Accept x
result Nothing = Pending
