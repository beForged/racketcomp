module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.IORef
import Data.List

data Val = Number Int
    |String String
    |Bool Bool
    |Atom String
    |List [Val]
--    |Lambda Val String Val --used in preprocessing
    
spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 

parseNumber :: Parser Val
parseNumber = (many1 digit) >>= \x -> return (Number (read x))

parseString :: Parser Val
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return (String x)

parseAtom :: Parser Val
parseAtom = do
    x <- letter <|> symbol
    xs <- many (letter <|> digit <|> symbol)
    let atom = x:xs
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseList :: Parser Val
parseList = (sepBy parseExpr spaces) >>= \x -> return (List x)

parseExpr :: Parser Val
parseExpr = parseNumber  
    <|> parseString
    <|> parseAtom
    <|> parseQuoted
    <|> do
        char '('
        x <- try parseList
        char ')'
        return x

parseQuoted :: Parser Val
parseQuoted = do
    char '\''
    x <- parseExpr
    return (List [Atom "quote", x])
unwords' :: [String] -> String
unwords' = concat . intersperse ", "

showVal :: Val -> String
--add ""?
showVal (String xs) = "String " ++ xs
showVal (Number n) = "Number" ++ (show n)
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Atom name) = "Atom " ++ name
showVal (List xs) = "(List [" ++ (unwords'.map showVal) xs ++ "])"
--showVal 

instance Show Val where show = showVal
