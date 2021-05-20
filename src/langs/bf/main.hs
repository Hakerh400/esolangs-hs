module Src.Langs.Bf.Main (main) where

import Data.Char
import Data.Bits
import Data.Word
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

type Parser = State String

-- instance Show Instruction where
--   show MoveLeft = "<"
--   show MoveRight = ">"
--   show Increment = "+"
--   show Decrement = "-"
--   show Input = ","
--   show Output = "."
--   show (Loop insts) = "[" ++ showProg insts ++ "]"
--
-- showProg :: Program -> String
-- showProg = concatMap show

data MachineT = MachineT {
  meml :: [Word8],
  memr :: [Word8],
  input :: String,
  output :: String}

type Machine = State MachineT

main :: String -> String -> String
main src input = let
  prog = evalState parse src
  machineInitial = initMachine input
  machineFinal = execState (runProg prog) machineInitial
  in output machineFinal

parse :: Parser Program
parse = do
  insts <- parseProg
  str <- get
  case str of
    "" -> return insts
    _ -> error "Missing open bracket"
  return insts

parseProg :: Parser Program
parseProg = do
  str <- get
  case str of
    "" -> return []
    (']':_) -> return []
    _ -> do
      instm <- parseInst
      insts <- parseProg
      return $ maybe insts (:insts) instm

parseInst :: Parser (Maybe Instruction)
parseInst = do
  (c:cs) <- get
  put cs
  case c of
    '<' -> return $ Just MoveLeft
    '>' -> return $ Just MoveRight
    '+' -> return $ Just Increment
    '-' -> return $ Just Decrement
    ',' -> return $ Just Input
    '.' -> return $ Just Output
    '[' -> do
      insts <- parseProg
      str <- get
      case str of
        (']':str') -> put str' >> (return $ Just (Loop insts))
        _ -> error str
    _ -> return Nothing

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
  m@(MachineT ml@(l:ml') mr@(r:mr') inp out) <- get
  case inst of
    MoveLeft -> put m {meml = r : ml, memr = mr'}
    MoveRight -> put m {meml = ml', memr = l : mr}
    Increment -> put m {memr = r + 1 : mr'}
    Decrement -> put m {memr = r - 1 : mr'}
    Input -> case out of
        [] -> put m {memr = 0 : mr'}
        (c:cs) -> put m {memr = fromIntegral (ord c) : mr', output = cs}
    Output -> put m {output = out ++ [chr (fromIntegral r)]}
    Loop insts -> loop insts

loop :: Program -> Machine ()
loop insts = do
  m <- get
  when (head (memr m) /= 0) $ do
    runProg insts
    loop insts