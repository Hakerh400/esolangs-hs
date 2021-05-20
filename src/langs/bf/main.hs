module Src.Langs.Bf.Main (main) where

import Data.Char
import Data.Bits
import Data.Functor.Identity
import Control.Monad.State

import Src.Util.Main

instance MonadFail Identity where
  fail = error

type Program = [Instruction]

data Instruction =
  MoveLeft |
  MoveRight |
  Increment |
  Decrement |
  Input |
  Output |
  Loop Program

instance Show Instruction where
  show MoveLeft = "<"
  show MoveRight = ">"
  show Increment = "+"
  show Decrement = "-"
  show Input = ","
  show Output = "."
  show (Loop insts) = "[" ++ showProg insts ++ "]"

type Pair a = (a, a)

data MachineT = MachineT {
  meml :: [Int],
  memr :: [Int],
  input :: String,
  output :: String}

type Machine = State MachineT

main :: String -> String -> String
main src input = let
  prog = parse src
  machineInitial = initMachine input
  machineFinal = execState (runProg prog) machineInitial
  output = extractOutput machineFinal
  in output

parse :: String -> Program
parse src = let
  (insts, chs) = parseProg src
  in case chs of
    "" -> insts
    _ -> error "Missing open bracket"

parseProg :: String -> (Program, String)
parseProg "" = ([], "")
parseProg src@(']':chs) = ([], src)
parseProg src = let
  (instm, chs) = parseInst src
  (insts, chs') = parseProg chs
  insts' = maybe2list instm ++ insts
  in (insts', chs')

parseInst :: String -> (Maybe Instruction, String)
parseInst (ch:chs) = case ch of
  '<' -> (Just MoveLeft, chs)
  '>' -> (Just MoveRight, chs)
  '+' -> (Just Increment, chs)
  '-' -> (Just Decrement, chs)
  ',' -> (Just Input, chs)
  '.' -> (Just Output, chs)
  '[' -> let
    (insts, chs') = parseProg chs
    in case chs' of
      (']':chs'') -> (Just (Loop insts), chs'')
      _ -> error "Missing closed bracket"
  _ -> (Nothing, chs)

initMachine :: String -> MachineT
initMachine inp = MachineT {
  meml = repeat 0,
  memr = repeat 0,
  input = inp,
  output = ""}

runProg :: Program -> Machine ()
runProg = mapM_ runInst

runInst :: Instruction -> Machine ()
runInst inst = do
  m <- get
  let (MachineT ml mr inp out) = m
  case inst of
    MoveLeft -> put $ m {meml = head mr : ml, memr = tail mr}
    MoveRight -> put $ m {meml = tail ml, memr = head ml : mr}
    Increment -> put $ m {memr = toByte (head mr + 1) : tail mr}
    Decrement -> put $ m {memr = toByte (head mr - 1) : tail mr}
    Input -> case out of
        [] -> put $ m {memr = 0 : tail mr}
        (c:cs) -> put $ m {memr = toByte (ord c) : tail mr, output = cs}
    Output -> put $ m {output = chr (head mr) : out}
    Loop insts -> loop insts

loop :: Program -> Machine ()
loop insts = do
  m <- get
  if head (memr m) /= 0
    then do
      runProg insts
      loop insts
    else return ()

extractOutput :: MachineT -> String
extractOutput m = reverse (output m)

showProg :: Program -> String
showProg = concatMap show

maybe2list :: Maybe a -> [a]
maybe2list = maybe [] (:[])

toByte :: Int -> Int
toByte = (.&. 255)