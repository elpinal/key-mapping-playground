module Mapping where

import Control.Applicative
import Data.Bifunctor

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
vFamily f = within Normal (Visual f) <||> withPred isVisual (toggleVisual f)

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

iwCmd :: Command
iwCmd = withPred isOperatorPending afterOperatorPending

within :: Mode -> Mode -> Command
within m n = withPred (== m) . const $ return n

withPred :: (Mode -> Bool) -> (Mode -> Maybe Mode) -> Command
withPred f g c =
  if f c
    then g c
    else Nothing

mnemonics :: [([Mod Char], Command)]
mnemonics = map (first toAlphabet)
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

toAlphabet :: String -> [Mod Char]
toAlphabet = map NoMod
