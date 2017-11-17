module Mapping where

import Control.Applicative

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
