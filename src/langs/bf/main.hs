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

data MachineT = MachineT
  (Pair [Int])
  (Pair String)

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
initMachine inp = MachineT
  (repeat 0, repeat 0)
  (inp, "")

runProg :: Program -> Machine ()
runProg = mapM_ runInst

runInst :: Instruction -> Machine ()
runInst inst = case inst of
  MoveLeft -> moveLeft
  MoveRight -> moveRight
  Increment -> increment
  Decrement -> decrement
  Input -> inputByte
  Output -> outputByte
  Loop insts -> loop insts

moveLeft :: Machine ()
moveLeft = do
  (ml, b : mr) <- getMem
  setMem (b : ml, mr)

moveRight :: Machine ()
moveRight = do
  (b : ml, mr) <- getMem
  setMem (ml, b : mr)

increment :: Machine ()
increment = do
  b <- getByte
  setByte (toByte $ b + 1)

decrement :: Machine ()
decrement = do
  b <- getByte
  setByte (toByte $ b - 1)

inputByte :: Machine ()
inputByte = do
  b <- readFromInput
  setByte b

outputByte :: Machine ()
outputByte = do
  b <- getByte
  writeToOutput b

loop :: Program -> Machine ()
loop insts = do
  b <- getByte
  if b /= 0
    then do
      runProg insts
      loop insts
    else return ()

readFromInput :: Machine Int
readFromInput = do
  inp <- getInput
  case inp of
    [] -> return 0
    (b : bs) -> do
      setInput bs
      return $ ord b 

writeToOutput :: Int -> Machine ()
writeToOutput b = do
  out <- getOutput
  setOutput (chr b : out)

getInput :: Machine String
getInput = do
  io <- getIO
  return $ fst io

getOutput :: Machine String
getOutput = do
  io <- getIO
  return $ snd io

getIO :: Machine (Pair String)
getIO = do
  (MachineT _ io) <- get
  return io

setInput :: String -> Machine ()
setInput inp = do
  (_, out) <- getIO
  setIO (inp, out)

setOutput :: String -> Machine ()
setOutput out = do
  (inp, _) <- getIO
  setIO (inp, out)

setByte :: Int -> Machine ()
setByte b = do
  mr <- getMemr
  setMemr (b : tail mr)

getByte :: Machine Int
getByte = do
  mr <- getMemr
  return $ head mr

setMemr :: [Int] -> Machine ()
setMemr mr = do
  (ml, _) <- getMem
  setMem (ml, mr)

setMem :: (Pair [Int]) -> Machine ()
setMem mem = do
  (MachineT _ io) <- get
  put (MachineT mem io)

setIO :: Pair String -> Machine ()
setIO io = do
  (MachineT mem _) <- get
  put (MachineT mem io)

getMemr :: Machine [Int]
getMemr = do
  mem <- getMem
  return $ snd mem

getMem :: Machine (Pair [Int])
getMem = do
  (MachineT mem _) <- get
  return mem

extractOutput :: MachineT -> String
extractOutput (MachineT _ (_, out)) = reverse out

showProg :: Program -> String
showProg = concatMap show

maybe2list :: Maybe a -> [a]
maybe2list = maybe [] (:[])

toByte :: Int -> Int
toByte = (.&. 255)