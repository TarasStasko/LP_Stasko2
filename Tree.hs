import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Control.Monad.Identity (Identity)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO.Unsafe (unsafePerformIO)

-- Типи даних
data Expr = Num Double
          | Var String
          | BinOp String Expr Expr
          | UnaryOp String Expr
          deriving (Show)

-- Лексичний аналізатор
lexer = makeTokenParser emptyDef {
    opStart = oneOf "+-*/^",
    opLetter = oneOf "+-*/^",
    reservedNames = ["mod", "sqrt"]
}

-- Парсери для атомарних елементів
number :: Parser Expr
number = do
    n <- naturalOrFloat lexer
    return $ case n of
        Left int -> Num (fromIntegral int)
        Right float -> Num float

variable :: Parser Expr
variable = do
    ident <- identifier lexer
    return $ Var ident

-- Основний парсер
expr :: Parser Expr
expr = buildExpressionParser operators term <?> "expression"

operators :: [[Operator String () Identity Expr]]
operators = [
    [Prefix (do reserved lexer "sqrt"
                return (\x -> UnaryOp "sqrt" x))],
    [Infix (reservedOp lexer "^" >> return (BinOp "exp")) AssocRight],
    [Infix (reservedOp lexer "*" >> return (BinOp "mul")) AssocLeft,
     Infix (reservedOp lexer "/" >> return (BinOp "dvd")) AssocLeft,
     Infix (reserved lexer "mod" >> return (BinOp "mod")) AssocLeft],
    [Infix (reservedOp lexer "+" >> return (BinOp "add")) AssocLeft,
     Infix (reservedOp lexer "-" >> return (BinOp "sub")) AssocLeft]
    ]

term :: Parser Expr
term = parens lexer expr
       <|> number
       <|> variable
       <?> "term"

-- Інтерпретатор
type Env = Map.Map String Double

eval :: Env -> Expr -> Either String (Double, Env)
eval env (Num x) = Right (x, env)
eval env (Var name) = case Map.lookup name env of
    Just val -> Right (val, env)
    Nothing -> Left $ "Undefined variable: " ++ name
eval env (BinOp op left right) = do
    (lVal, env1) <- eval env left
    (rVal, env2) <- eval env1 right
    case op of
        "add" -> Right (lVal + rVal, env2)
        "sub" -> Right (lVal - rVal, env2)
        "mul" -> Right (lVal * rVal, env2)
        "dvd" -> if rVal == 0 
                then Left "Division by zero" 
                else Right (lVal / rVal, env2)
        "exp" -> Right (lVal ** rVal, env2)
        "mod" -> if rVal == 0
                then Left "Modulo by zero"
                else Right (fromIntegral (floor lVal `mod` floor rVal), env2)
        _ -> Left $ "Unknown operator: " ++ op
eval env (UnaryOp "sqrt" ex) = do
    (val, newEnv) <- eval env ex
    if val >= 0 
        then Right (sqrt val, newEnv)
        else Left "Negative sqrt argument"

-- Функції для виводу AST
showAST :: Expr -> String
showAST (Num x) = show x
showAST (Var name) = name
showAST (BinOp op left right) = op ++ "(" ++ showAST left ++ "," ++ showAST right ++ ")"
showAST (UnaryOp "sqrt" ex) = "sqrt(" ++ showAST ex ++ ")"

-- Вимірювання часу виконання
timeIt :: IO a -> IO (a, Double)
timeIt action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let time = realToFrac (diffUTCTime end start) * 1000 -- у мілісекундах
    return (result, time)

-- Головна функція
parseAndEval :: String -> IO (Either String (String, Double, Double))
parseAndEval input = do
    let parseResult = parse (contents expr) "" input
    case parseResult of
        Left parseErr -> return $ Left $ show parseErr
        Right ast -> do
            (evalResult, time) <- timeIt $ return $ eval Map.empty ast
            case evalResult of
                Left evalErr -> return $ Left evalErr
                Right (result, _) -> return $ Right (showAST ast, result, time)

contents :: Parser a -> Parser a
contents p = do
    whiteSpace lexer
    r <- p
    eof
    return r

-- Тестові вирази
testCases = [
    "((2^(3+1)+sqrt(16*4))*(5 mod 3))/(2-1)^2",
    "(2 + 3) * 4", 
    "2.5 * (3 + 1)",
    "5 mod 2",
    "sqrt(16) * 2"
    ]

-- Функція для тестування
runTests :: IO ()
runTests = mapM_ test testCases
    where
        test expr = do
            putStrLn $ "\nТестуємо: " ++ expr
            result <- parseAndEval expr
            case result of
                Left err -> putStrLn $ "Помилка: " ++ err
                Right (ast, res, time) -> do
                    putStrLn $ "AST: " ++ ast
                    putStrLn $ "Результат: " ++ show res
                    putStrLn $ "Час обчислення: " ++ show time ++ " мс"

-- Запуск тестів
main :: IO ()
main = runTests