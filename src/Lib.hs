module Lib where

import qualified Data.Map.Lazy as Map
import Data.Maybe
import Safe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Alphabet = Alphabet Char deriving (Show, Eq, Ord)

data Mode = Normal
          | Insert
          deriving (Show, Eq, Ord)

data Command = Name String
             | Enter Mode
             deriving (Show, Eq)

data Mapping = Mapping [Alphabet] [Alphabet]

type Mappings = Map.Map [Alphabet] [Alphabet]

type Synonyms = Map.Map [Alphabet] [Command]

modeMap :: Map.Map Mode Synonyms
modeMap = Map.fromList
  [ (Normal, normalSynonyms)
  , (Insert, insertSynonyms)
  ]

-- Note: Don't use empty list for keys.
normalSynonyms :: Synonyms
normalSynonyms = Map.fromList
                  [ ([Alphabet 'h'], [Name "moveToLeft"])
                  , ([Alphabet 'j'], [Name "moveDown"])
                  , ([Alphabet 'k'], [Name "moveUp"])
                  , ([Alphabet 'l'], [Name "moveToRight"])
                  , ([Alphabet 'h', Alphabet '!'], [Name "h!"])
                  , ([Alphabet 'h', Alphabet 'i', Alphabet '!'], [Name "hi!"])
                  , ([Alphabet 'g', Alphabet 'u'], [Name "toLowerCase"])
                  , ([Alphabet 'i'], [Enter Insert])]

insertSynonyms :: Synonyms
insertSynonyms = Map.fromList
  [ ([Alphabet 'y'], [Name "input 'y'"])
  , ([Alphabet '\ESC'], [Enter Normal])
  ]

data Result = Accept [Command]
            | Pending [Alphabet]
            deriving (Show, Eq)

translateWithMappings :: Synonyms -> Mappings -> [Alphabet] -> [Result]
translateWithMappings _ _ [] = []
translateWithMappings m ma xs = translateN m m (translateMapping ma xs) 0

translate :: Synonyms -> [Alphabet] -> [Result]
translate m xs = translateWithMappings m Map.empty xs

translateN :: Synonyms -> Synonyms -> [Alphabet] -> Int -> [Result]
translateN original ps xs n =
  let
    xs' = drop n xs
    ps' = Map.filterWithKey (\ks _ -> fromMaybe False $ (==) <$> headMay xs' <*> ks `atMay` n) ps

    c0 = Accept <$> Map.lookup (take n xs) ps
    c = Map.lookup (take (n+1) xs) ps'
  in
    case length ps' of
      0 -> backtrack c0 xs n original
      1 | isJust c -> result c xs : translate (selectMappings original $ Accept $ fromJust c) (tail xs')
      _ -> if length xs == n+1 then [result c xs] else translateN original ps' xs (n+1)

backtrack :: Maybe Result -> [Alphabet] -> Int -> Synonyms -> [Result]
backtrack c0 xs n original = if isJust c0 then fromJust c0 : translate (selectMappings original (fromJust c0)) (drop n xs) else
  let
    c1 = Accept <$> Map.lookup (take (n-1) xs) original
  in
    if n == 0 then translate original (tail xs) else backtrack c1 xs (n-1) original

result :: Maybe [Command] -> [Alphabet] -> Result
result (Just x) _ = Accept x
result Nothing xs = Pending xs

selectMappings :: Synonyms -> Result -> Synonyms
selectMappings _ (Pending x) = error $ "selectMappings: " ++ show x
selectMappings original (Accept xs) = lastDef original $ mapMaybe select xs
  where
    select (Name _) = Nothing
    select (Enter m) = Just $ Map.findWithDefault (error $ "no such mode in mode map: " ++ show m) m modeMap

mapping :: [Mapping] -> Mappings
mapping xs = mapping' xs Map.empty
  where
    mapping' [] m = m
    mapping' ((Mapping lhs rhs):xs) m = mapping' xs $ Map.insert lhs rhs m

translateMapping :: Mappings -> [Alphabet] -> [Alphabet]
translateMapping m xs = translateMappingN m m xs 0

translateMappingN :: Mappings -> Mappings -> [Alphabet] -> Int -> [Alphabet]
translateMappingN original ma xs n =
  let
    xs' = drop n xs
    ma' = Map.filterWithKey (\ks _ -> fromMaybe False $ (==) <$> headMay xs' <*> ks `atMay` n) ma

    c0 = Map.lookup (take n xs) ma
    c = Map.lookup (take (n+1) xs) ma'
  in
    case length ma' of
      0 -> backtrackMapping c0 xs n original
      1 | isJust c -> fromJust c ++ tail xs'
      _ -> if length xs == n+1 then fromMaybe xs c else translateMappingN original ma' xs (n+1)

backtrackMapping :: Maybe [Alphabet] -> [Alphabet] -> Int -> Mappings -> [Alphabet]
backtrackMapping c0 xs n original = if isJust c0 then fromJust c0 ++ drop n xs else
  let
    c1 = Map.lookup (take (n-1) xs) original
  in
    if n == 0 then xs else backtrackMapping c1 xs (n-1) original
