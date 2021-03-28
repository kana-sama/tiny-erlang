import Data.Proxy
import Language.TinyErlang.Interpreter (eval)
import Language.TinyErlang.Lexer (lex)
import Language.TinyErlang.Parser
import TEPrelude

x :: Proxy Int
x = do
  Proxy
  return 1

main :: IO ()
main = eval . parse . lex =<< readFile "example.erl"
