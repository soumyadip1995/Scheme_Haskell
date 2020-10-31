import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad 
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Double
             
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- Rewrite parseNumber, without liftM, using
-- do-notation
-- explicit sequencing with the >>= operator 

-- parseNumber :: Parser LispVal
-- parseNumber = do

--   x <- many1 digit
--   (return . Number . read) x

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool
        <|> parseFloat
parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin


parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do

  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

-- ### Solution for floating point Numbers
parseFloat :: Parser LispVal
parseFloat = do

  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst.head$readFloat (x++"."++y))


parseHex :: Parser LispVal
parseHex = do

  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)


parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)
parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)


-- ghc main.hs && main "\"this is shreya\" 



