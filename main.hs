import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"



readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

