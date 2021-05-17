import System.IO

import qualified Src.Langs.Bf.Main as Prog

main :: IO ()
main = do
  mapM (flip hSetBuffering NoBuffering) [stdin, stdout, stderr]

  let output = Prog.main src input
  putStrLn output

  return ()

src :: String
src = "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."

input :: String
input = "abc123"