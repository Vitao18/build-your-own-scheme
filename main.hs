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
             | Binary String

instance Show LispVal where show = showVal

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseVector :: Parser LispVal
parseVector = do
  char '#'
  parseList

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

{- parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit -}

{- parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ Number (read x) -}

parseNumber :: Parser LispVal
parseNumber =  (many1 digit) >>= (return . Number . read)

parseBinary :: Parser LispVal
parseBinary = do
  char '#'
  char 'b'
  x <- many (oneOf ("10"))
  return $ Binary x
  

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseBinary
  <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

-- <|> between (char '(') (char ')') (try parseList <|> parseDottedList) 

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Bool b)       = if b == True then "#t" else "#f"
showVal (String s)     = "\"" ++ s ++ "\""
showVal (Number n)     = show n
showVal (Atom a)       = a
showVal (List l)       = "(" ++ unwordsList l ++ ")"
showVal (DottedList l v) = "(" ++ unwordsList l ++ " . " ++ (showVal v) ++ ")"

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", isNumber),
              ("string?", isString),
              ("boolean?", isBoolean),
              ("list?", isList)]

isList :: [LispVal] -> LispVal
isList [(List _)]    = Bool True
isList _             = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [(Bool _)] = Bool True
isBoolean _          = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _            = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)]  = Bool True
isNumber _             = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

eval :: LispVal -> LispVal
eval val@(String _)       = val
eval val@(Bool   _)       = val
eval val@(Atom   _)       = val
eval val@(Binary _)       = val
eval val@(Number _)       = val
--eval val@(List   l)       = List (map eval l)
eval val@(DottedList h t) = DottedList (map eval h) (eval t)
eval (List (Atom func : args)) = apply func $ map eval args

main :: IO ()
main = getArgs >>= print . eval . readExpr . head


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

