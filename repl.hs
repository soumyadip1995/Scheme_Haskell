
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Numeric
import System.IO


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             deriving Show 

--showing typeclass

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



parseNumber :: Parser LispVal

parseNumber = do
  x <- many1 digit
  return $ Number $ read x

-- parseNumber = liftM (Number . read) $ many1 digit



parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit 
  char '.'
  y <- many1 digit
  let atom = (x ++ "." ++ y)
  return $ Float (fst.head$readFloat (x++"."++y))





parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail


parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

---- evaluator

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
--                            if null parsed 
--                               then 0
--                               else fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
-- unpackNum _ = 0

unpackNum (Bool True) = 1
unpackNum (Bool False) = 0


unpackStr :: LispVal -> String
unpackStr (String s) = s


unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

compareBinop [Atom x, Atom y] = (Bool (x==y))
compareBinop _ = (Bool False)

boolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinop unpacker op [x, y] = Bool $ (unpacker x) `op` (unpacker y)

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("eq?", compareBinop)]



apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


eval :: LispVal -> LispVal
eval val@(Atom _) = val

eval val@(String _) = val
eval val@(Number _) = val

eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- repl--

--Then, we create a function that prints out a prompt and reads in a 
--line of input:

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: String -> IO String
evalString expr = return $ show $ eval $ readExpr expr

--evaluate the string and print the result
evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

--Now it's time to tie it all together. We want to read input, perform a function, 
--and print the output

-- until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

main :: IO ()

main = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

