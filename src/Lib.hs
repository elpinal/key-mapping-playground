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

type Synonym = Map.Map [Alphabet] [Command]

modeMap :: Map.Map Mode Synonym
modeMap = Map.fromList
  [ (Normal, normalMappings)
  , (Insert, insertMappings)
  ]

-- Note: Don't use empty list for keys.
normalMappings :: Synonym
normalMappings = Map.fromList
                  [ ([Alphabet 'h'], [Name "moveToLeft"])
                  , ([Alphabet 'j'], [Name "moveDown"])
                  , ([Alphabet 'k'], [Name "moveUp"])
                  , ([Alphabet 'l'], [Name "moveToRight"])
                  , ([Alphabet 'h', Alphabet '!'], [Name "h!"])
                  , ([Alphabet 'h', Alphabet 'i', Alphabet '!'], [Name "hi!"])
                  , ([Alphabet 'g', Alphabet 'u'], [Name "toLowerCase"])
                  , ([Alphabet 'i'], [Enter Insert])]

insertMappings :: Synonym
insertMappings = Map.fromList
  [ ([Alphabet 'y'], [Name "input 'y'"])
  , ([Alphabet '\ESC'], [Enter Normal])
  ]

data Result = Accept [Command]
            | Pending
            deriving (Show, Eq)

translate :: Synonym -> [Alphabet] -> [Result]
translate _ [] = []
translate m xs = translateN m m xs 0

translateN :: Synonym -> Synonym -> [Alphabet] -> Int -> [Result]
translateN original ps xs n =
  let
    xs' = drop n xs
    ps' = Map.filterWithKey (\ks _ -> fromMaybe False $ (==) <$> headMay xs' <*> ks `atMay` n) ps

    c0 = Accept <$> Map.lookup (take n xs) ps
    c = Map.lookup (take (n+1) xs) ps'
  in
    case length ps' of
      0 -> backtrack c0 xs n original
      1 | isJust c -> result c : translate (selectMappings original $ Accept $ fromJust c) (tail xs')
      _ -> if length xs == n+1 then [result c] else translateN original ps' xs (n+1)

backtrack :: Maybe Result -> [Alphabet] -> Int -> Synonym -> [Result]
backtrack c0 xs n original = if isJust c0 then fromJust c0 : translate (selectMappings original (fromJust c0)) (drop n xs) else
  let
    c1 = Accept <$> Map.lookup (take (n-1) xs) original
  in
    if n == 0 then translate original (tail xs) else backtrack c1 xs (n-1) original

result :: Maybe [Command] -> Result
result (Just x) = Accept x
result Nothing = Pending

selectMappings :: Synonym -> Result -> Synonym
selectMappings _ Pending = error "unexpected Pending"
selectMappings original (Accept xs) = lastDef original $ mapMaybe select xs
  where
    select (Name _) = Nothing
    select (Enter m) = Just $ Map.findWithDefault (error $ "no such mode in mode map: " ++ show m) m modeMap
