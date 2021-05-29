module Main where

{--
Grammar:
prog = expr
expr = term ((PLUS | MINUS) term)*
term = factor ((MUL | DIV) factor)*
factor = NUM
--}

import Data.Char (isDigit)

data TkType = T_Int
            | T_Op
            | T_Lparen
            | T_Rparen
            deriving (Show)

data Tk = Tk { name  :: TkType 
             , value :: String }

instance Show Tk where
  show (Tk n v) = " "++show n++" "++v++"\n"

isOperator :: Char -> Bool
isOperator = flip elem ['+', '-', '*', '/']

-- Lexer
lexer :: String -> [Tk]
lexer "" = []
lexer (x:xs)
  | isDigit x    = Tk T_Int (x : takeWhile isDigit xs) : lexer (dropWhile isDigit xs)
  | isOperator x = Tk T_Op [x] : lexer xs
  | x == '('     = Tk T_Lparen [x] : lexer xs
  | x == ')'     = Tk T_Rparen [x] : lexer xs
  | otherwise    = error "Unknown symbol" 

data AST = Num    Float
         | Plus   AST AST
         | Minus  AST AST
         | Mul    AST AST
         | Div    AST AST
         | UMinus AST
         | Paren  AST
         deriving (Show)

expr :: [Tk] -> (AST, [Tk])
expr [] = error "Unexpected EOF while parsing"

expr (Tk T_Int v : Tk T_Op op : xs) =
  let e1 = Num (read v)
      (e2, xs') = term xs
  in  case op of
        "+" -> (Plus e1 e2, xs')
        "-" -> (Minus e1 e2, xs')

expr (Tk T_Op "-" : xs) =
  let (e, xs') = expr xs
  in  (UMinus e, xs')

expr (Tk T_Int v : xs) = (Num (read v), xs)
expr _ = error "Syntax error"

term :: [Tk] -> (AST, [Tk])
term [] = error "Unexpected EOF while parsing"
term (Tk T_Int v : Tk T_Op op : xs) =
  let e1 = Num (read v)
      (e2, xs') = factor xs
  in  case op of
        "*" -> (Mul e1 e2, xs')
        "/" -> (Div e1 e2, xs')

term (Tk T_Int v : xs) = (Num (read v), xs)
term _ = error "Syntax error"

factor :: [Tk] -> (AST, [Tk])
factor [] = error "Unexpected EOF while parsing"
factor (Tk T_Int v : xs) = (Num (read v), xs)

{-
 - Grammar
 - expr = NUM ((PLUS | MINUS) NUM)*
 -}
{-
$ 2 + 2 * 3 + 4
=> WRONG   (2 + (2 * (3 + 4)))
=> CORRECT (2 + ((2 * 3) + 4))
=> CORRECT (+ 2 (+ (* 2 3) 4))

--}

eval :: AST -> Float
eval (Num x) = x
eval (Plus e1 e2) =
  let n1 = eval e1
      n2 = eval e2
  in  n1 + n2
eval (Minus e1 e2) =
  let n1 = eval e1
      n2 = eval e2
  in  n1 - n2
eval (Mul e1 e2) =
  let n1 = eval e1
      n2 = eval e2
  in  n1 * n2
eval (Div e1 e2) =
  let n1 = eval e1
      n2 = eval e2
  in  n1 / n2
eval (UMinus e) =
  let n = eval e
  in  -n

--TESTING
ast :: String -> (AST, [Tk])
ast = expr . lexer
calc :: String -> Float
calc = eval . fst . ast


main :: IO ()
main = putStrLn "Hello, world!"
























