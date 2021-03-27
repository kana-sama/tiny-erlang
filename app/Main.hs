import Language.TinyErlang.Interpreter (eval)
import Language.TinyErlang.Lexer (lex)
import Language.TinyErlang.Parser
import TEPrelude

main :: IO ()
main = eval . parse . lex =<< readFile "example.erl"
