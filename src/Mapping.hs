module Mapping where

import Control.Applicative
import Data.Bifunctor
import qualified Data.Map.Lazy as Map

type Command = Mode -> Maybe Mode

data Mode =
    Normal
  | Visual Form
  | Insert
  | OperatorPending Mode
  deriving (Eq, Show)

data Form =
    Block
  | Line
  | Character
  deriving (Eq, Show)

enter :: Mode -> Mode -> Maybe Mode
enter = pure . return

isOperatorPending :: Mode -> Bool
isOperatorPending (OperatorPending _) = True
isOperatorPending _ = False

isVisual :: Mode -> Bool
isVisual (Visual _) = True
isVisual _ = False

afterOperatorPending :: Mode -> Maybe Mode
afterOperatorPending (OperatorPending m) = return m
afterOperatorPending _ = Nothing

iCmd :: Command
iCmd = within Normal Insert

jCmd :: Command
jCmd = within Normal Normal

-- The suffix 'C' means 'capital', so iCmdC means 'capital "i" command' ("I" command).
iCmdC :: Command
iCmdC = within Normal Insert <||> withPred isVisual (enter Insert)

vFamily :: Form -> Command
vFamily = within Normal . Visual <|||> withPred isVisual . toggleVisual

vCmd :: Command
vCmd = vFamily Character

vCmdC :: Command
vCmdC = vFamily Line

vCmdCtl :: Command
vCmdCtl = vFamily Block

toggleVisual :: Form -> Mode -> Maybe Mode
toggleVisual target (Visual form)
  | form == target = return Normal
  | otherwise      = return $ Visual target
toggleVisual _ _ = Nothing

infixl 3 <||>
(<||>) = liftA2 (<|>)

infixl 3 <|||>
(<|||>) = liftA2 (<||>)

iwCmd :: Command
iwCmd = withPred isOperatorPending afterOperatorPending

within :: Mode -> Mode -> Command
within m n = withPred (== m) . const $ return n

withPred :: (Mode -> Bool) -> (Mode -> Maybe Mode) -> Command
withPred f g c =
  if f c
    then g c
    else Nothing

mnemonics :: Map.Map [Mod Char] Command
mnemonics = Map.fromList $ map (first toAlphabet)
  [ ("i", iCmd)
  , ("iw", iwCmd)
  , ("j", jCmd)
  , ("v", vCmd)
  , ("I", iCmdC)
  , ("V", vCmdC)
  ] ++ [([Ctl 'v'], vCmdCtl)]

data Mod a =
    Ctl a
  | NoMod a
  deriving (Eq, Ord, Show)

toAlphabet :: String -> [Mod Char]
toAlphabet = map NoMod

data F a b =
    K (Maybe b) (a -> F a b)
  | Z (Maybe b) -- might not be needed? overlapped with K?

lookupF :: [a] -> F a b -> (Maybe b, [a])
lookupF xs (Z y) = (y, xs)
lookupF (x : xs) (K def f) = case lookupF xs $ f x of
  a @ (Just _, _) -> a
  (Nothing, vs) -> (def, x : vs)
lookupF [] (K def f) = (def, [])

buildCommands :: Map.Map [Mod Char] Command -> F (Mod Char) Command
buildCommands = K Nothing . f
  where
    f :: Map.Map [Mod Char] Command -> Mod Char -> F (Mod Char) Command
    f m mc =
      let
        m' = Map.filterWithKey (\k _ -> Just mc == headMay k) m
      in
        K (Map.lookup [mc] m') . f $ Map.mapKeys tail m'

headMay :: [a] -> Maybe a
headMay (x : xs) = Just x
headMay [] = Nothing

execute :: [Mod Char] -> (Maybe Command, [Mod Char])
execute xs = lookupF xs $ buildCommands mnemonics

executeAll :: [Mod Char] -> ([Command], [Mod Char])
executeAll xs = case execute xs of
  (Just c, ys) -> first (c :) $ executeAll ys
  (Nothing, ys) -> ([], ys)

data Env = Env
  { transMap :: Map.Map [Mod Char] [Mod Char]
  , noreMap  :: Map.Map [Mod Char] Command
  }

type EnvTransformer = Env -> Env

initialEnv :: Env
initialEnv = Env
  { transMap = Map.empty
  , noreMap = Map.empty
  }

translate :: [Mod Char] -> [Mod Char] -> EnvTransformer
translate s d e = e { transMap = Map.insert s d $ transMap e }

seqCmd :: [Command] -> Command
seqCmd (c : cs) m = c m >>= seqCmd cs
seqCmd [] m = return m

noremap :: [Mod Char] -> [Mod Char] -> EnvTransformer
noremap s d e = e { noreMap = Map.insert s (seqCmd . fst $ executeAll d) $ noreMap e }
