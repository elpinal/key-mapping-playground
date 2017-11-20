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
  deriving Eq

data Form =
    Block
  | Line
  | Character
  deriving Eq

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
  deriving (Eq, Ord)

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

buildCommands :: Map.Map [Mod Char] Command -> F (Mod Char) Command
buildCommands m = K Nothing f
  where
    f :: Mod Char -> F (Mod Char) Command
    f mc =
      let
        m' = Map.filterWithKey (\k a -> mc `isPrefixOf` k) m
      in
        K (Map.lookup [mc] m') f

isPrefixOf :: Eq a => a -> [a] -> Bool
isPrefixOf x (y : ys) | x == y = True
isPrefixOf _ _ = False

execute :: [Mod Char] -> Maybe (Command, [Mod Char])
execute (x : xs) =
  case Map.lookup [x] mnemonics of
    Just c -> Just (c, xs)
    Nothing -> Nothing
