module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

{- parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit -}

{- parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ Number (read x) -}

parseNumber :: Parser LispVal
parseNumber =  (many1 digit) >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found Value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr


{- main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1) -}

{- main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show (2 * (read (args !! 0) :: Int)) -}

{- main :: IO ()
main = do
  putStrLn "Enter something and we will make it rocks!"
  input <- getLine
putStrLn $ input ++ "!!" -}

