module Src.Langs.Bf.Main (main) where

import Data.Char
import Data.Bits
import Control.Monad.State
import Data.Functor.Identity

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

data Machine = Machine {
  mem_ :: ([Int], [Int]),
  inp_ :: String,
  out_ :: String}

type Context = State Machine

main :: String -> String -> String
main src input = let
  prog = parse src
  machineInitial = initMachine input
  machineFinal = execState (runProg prog) machineInitial
  output = out_ machineFinal
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

initMachine :: String -> Machine
initMachine inp = Machine {
  mem_ = (repeat 0, repeat 0),
  inp_ = inp,
  out_ = ""}

runProg :: Program -> Context ()
runProg = mapM_ runInst

runInst :: Instruction -> Context ()
runInst inst = do
  (Machine (ml@(l:ls), mr@(r:rs)) inp out) <- get
  case inst of
    MoveLeft -> put $ Machine (ls, (l : mr)) inp out
    MoveRight -> put $ Machine ((r : ml), rs) inp out
    Increment -> put $ Machine (ml, ((r + 1) .&. 255 : rs)) inp out
    Decrement -> put $ Machine (ml, ((r - 1) .&. 255 : rs)) inp out
    Input -> do
      let (b, bs) = readByte inp
      put $ Machine (ml, (b : rs)) bs out
    Output -> put $ Machine (ml, mr) inp (out ++ [chr r])
    Loop insts -> if r == 0 then
      return ()
      else do
        runProg insts
        runInst inst

readByte :: String -> (Int, String)
readByte [] = (0, [])
readByte (c:cs) = (ord c, cs)

showProg :: Program -> String
showProg = concatMap show

maybe2list :: Maybe a -> [a]
maybe2list = maybe [] (:[])