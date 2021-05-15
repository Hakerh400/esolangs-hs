import qualified Src.Langs.Bf.Main as Prog

main :: IO ()
main = do
  let output = Prog.main src input
  putStrLn output
  return ()

src :: String
src = "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."

input :: String
input = "abc123"