module Main where

import Data.Char (isDigit)

-----------
-- Lexer --
-----------

data TkType = TInt
            | TOp
            | TLparen
            | TRparen
            | TEof
            deriving (Show)

data Tk = Tk { name  :: TkType 
             , value :: String }

instance Show Tk where
  show (Tk n v) = " "++(show n)++" "++v++"\n"

isOperator :: Char -> Bool
isOperator = flip elem ['+', '-', '*', '/']

scan :: String -> [Tk]
scan "" = [Tk TEof ""]
scan (x:xs)
  | isDigit x    = Tk TInt (x : takeWhile isDigit xs) : scan (dropWhile isDigit xs)
  | isOperator x = Tk TOp [x] : scan xs
  | x == '('     = Tk TLparen [x] : scan xs
  | x == ')'     = Tk TRparen [x] : scan xs
  | otherwise    = error "Unknown symbol" 

------------
-- Parser --
------------

-- AST: Abstract Syntax Tree
data AST = Num Float
         | Plus AST AST
         | Minus AST AST
         | Mul AST AST
         | Div AST AST
         | UMinus AST
         | Paren AST
         deriving (Show)

{-
  Grammar:
    program = expr
    expr = term ((PLUS | MINUS) term)*
    term = factor ((MUL | DIV) factor)*
    factor = NUM | LPAREN expr RPAREN
-}

expr [Tk TEof ""] = error "Unexpected EOF while parsing"
expr xs = 
  let (e1, xs') = term xs
      op = value (head xs') -- Peeks a token and takes its value
  in  case op of
        "+" -> let (e2, xs'') = expr (tail xs') in (Plus e1 e2, xs'')
        "-" -> let (e2, xs'') = expr (tail xs') in (Minus e1 e2, xs'')
        _   -> (e1, xs') -- No operator left, return the AST

term [Tk TEof ""] = error "Unexpected EOF while parsing"
term xs = 
  let (e1, xs') = factor xs
      op = value (head xs')
  in  case op of
        "*" -> let (e2, xs'') = term (tail xs') in (Mul e1 e2, xs'')
        "/" -> let (e2, xs'') = term (tail xs') in (Div e1 e2, xs'')
        _    -> (e1, xs')

factor [Tk TEof ""] = error "Unexpected EOF while parsing"
factor (Tk TInt v : xs) = (Num (read v), xs)
factor (Tk TLparen _ : xs) =
  let (e, xs') = expr xs
      t = name $ head xs'
  in  case t of
        TRparen -> (e, tail xs')
        _       -> error "Unclosed parenthesis"
        
-----------------
-- Interpreter --
-----------------

-- Evaluates an AST
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

-------------
-- TESTING --
-------------

ast = expr . scan
calc = eval . fst . ast

test :: Bool
test =
  calc "1+1"         == 2.0  &&
  calc "1+2*3"       == 7.0  &&
  calc "2*3+1"       == 7.0  &&
  calc "2*(3+3)"     == 12.0 &&
  calc "2+(3+3)"     == 8.0  &&
  calc "2+(3+1)*2+4" == 14.0


----------------
-- Dummy main --
----------------
 
main :: IO ()
main = putStrLn "Hello, Haskalc!"
