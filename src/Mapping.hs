module Mapping where

import Control.Applicative

type Command = Mode -> Maybe Mode

newtype Mode = Mode String
  deriving Eq

normalMode :: Mode
normalMode = Mode "normal"

visualMode :: Mode
visualMode = Mode "visual"

insertMode :: Mode
insertMode = Mode "insert"

iCmd :: Command
iCmd = within normalMode insertMode

jCmd :: Command
jCmd = within normalMode normalMode

-- The suffix 'C' means 'capital', so iCmdC means 'capital "i" command' ("I" command).
iCmdC :: Command
iCmdC = within normalMode insertMode <||> within visualMode insertMode

(<||>) = liftA2 (<|>)

within :: Mode -> Mode -> Command
within m n c =
  if c == m
    then return n
    else Nothing
