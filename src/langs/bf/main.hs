module Src.Langs.Bf.Main (main) where

import Data.Char
import Data.Bits

import Src.Util.Main

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

data Context = Context {
  mem_ :: ([Int], [Int]),
  inp_ :: String,
  out_ :: String}

main :: String -> String -> String
main src input = let
  prog = parse src
  ctxInitial = initCtx input
  ctxFinal = run ctxInitial prog
  output = out_ ctxFinal
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

initCtx :: String -> Context
initCtx inp = Context {
  mem_ = (repeat 0, repeat 0),
  inp_ = inp,
  out_ = ""}

run :: Context -> Program -> Context
run ctx [] = ctx
run ctx (inst:insts) = let
  ctx' = runInst ctx inst
  in run ctx' insts

runInst :: Context -> Instruction -> Context
runInst ctx inst = let
  (Context (ml@(l:ls), mr@(r:rs)) inp out) = ctx
  in case inst of
    MoveLeft -> Context (ls, (l : mr)) inp out
    MoveRight -> Context ((r : ml), rs) inp out
    Increment -> Context (ml, ((r + 1) .&. 255 : rs)) inp out
    Decrement -> Context (ml, ((r - 1) .&. 255 : rs)) inp out
    Input -> let
      (b, bs) = readByte inp
      in Context (ml, (b : rs)) bs out
    Output -> Context (ml, mr) inp (out ++ [chr r])
    Loop insts -> case r of
      0 -> ctx
      _ -> let
        ctx' = run ctx insts
        in runInst ctx' inst

readByte :: String -> (Int, String)
readByte [] = (0, [])
readByte (c:cs) = (ord c, cs)

showProg :: Program -> String
showProg = concatMap show

maybe2list :: Maybe a -> [a]
maybe2list = maybe [] (:[])