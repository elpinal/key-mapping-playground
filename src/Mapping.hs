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

execute :: [Mod Char] -> Maybe (Command, [Mod Char])
execute (x : xs) =
  case Map.lookup [x] mnemonics of
    Just c -> Just (c, xs)
    Nothing -> Nothing
