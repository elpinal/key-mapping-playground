module Lib
    ( someFunc
    ) where

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
            deriving (Show)

translate :: [Alphabet] -> [Result]
translate [] = []
translate (x:xs) =
  let
    ps = Map.filterWithKey (\(k:ks) _ -> x == k) commandMappings

    c1 = Map.lookup [x] ps
  in
    case length ps of
      0 -> maybeToList (Accept <$> Map.lookup [] ps) ++ translate xs
      1 | isJust c1 -> result c1 : translate xs
      _ -> if null xs then [result (Map.lookup [x] ps)] else translate' ps (x:xs)

translate' :: Map.Map [Alphabet] Command -> [Alphabet] -> [Result]
translate' ps (x:xs) =
  let
    ps' = Map.filterWithKey (\(k:ks) _ -> fromMaybe False $ (==) <$> headMay xs <*> headMay ks) ps

    c2 = Map.lookup [x, head xs] ps'
  in
    case length ps' of
      0 -> maybeToList (Accept <$> Map.lookup [x] ps) ++ translate xs
      1 | c2 /= Nothing -> result c2 : translate (tail xs)
      _ -> if null (tail xs) then [result (Map.lookup [x, head xs] ps')] else translate'' ps' (x:xs)

translate'' :: Map.Map [Alphabet] Command -> [Alphabet] -> [Result]
translate'' ps (x:xs) =
  let
    xs' = tail xs
    ps' = Map.filterWithKey (\(k:ks) _ -> fromMaybe False $ (==) <$> headMay xs' <*> ks `atMay` 1) ps

    c3 = Map.lookup [x, head xs, head xs'] ps'
  in
    case length ps' of
      0 -> maybeToList (Accept <$> Map.lookup [x, head xs] ps) ++ translate xs'
      1 | c3 /= Nothing -> result c3 : translate (tail xs')
      _ -> if null (tail xs') then [result $ Map.lookup [x, head xs, head xs'] ps'] else translate''' ps' (x:xs)

translate''' :: Map.Map [Alphabet] Command -> [Alphabet] -> [Result]
translate''' ps (x:xs) =
  let
    xs' = drop 2 xs
    ps' = Map.filterWithKey (\(k:ks) _ -> fromMaybe False $ (==) <$> headMay xs' <*> ks `atMay` 2) ps

    c4 = Map.lookup (x : take 3 xs) ps'
  in
    case length ps' of
      0 -> maybeToList (Accept <$> Map.lookup (x : take 2 xs) ps) ++ translate xs'
      1 | isJust c4 -> result (Map.lookup (x : take 3 xs) ps') : translate (drop 3 xs)
      _ -> undefined

result :: Maybe Command -> Result
result (Just x) = Accept x
result Nothing = Pending
