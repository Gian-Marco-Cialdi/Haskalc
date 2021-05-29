module Main where

import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe)

{--
Grammar:
prog = expr
expr = term ((PLUS | MINUS) term)*
term = factor ((MUL | DIV) factor)*
factor = NUM
--}

import Data.Char (isDigit)

data TkType = TInt
            | TOp
            | TLparen
            | TRparen
            deriving (Show)

data Tk = Tk { name  :: TkType 
             , value :: String }

instance Show Tk where
  show (Tk n v) = " "++(show n)++" "++v++"\n"

isOperator :: Char -> Bool
isOperator = flip elem ['+', '-', '*', '/']

-- Lexer
lexer :: String -> [Tk]
lexer "" = []
lexer (x:xs)
  | isDigit x    = Tk TInt (x : takeWhile isDigit xs) : lexer (dropWhile isDigit xs)
  | isOperator x = Tk TOp [x] : lexer xs
  | x == '('     = Tk TLparen [x] : lexer xs
  | x == ')'     = Tk TRparen [x] : lexer xs
  | otherwise    = error "Unknown symbol" 

data AST = Num Float
         | Plus AST AST
         | Minus AST AST
         | Mul AST AST
         | Div AST AST
         | UMinus AST
         | Paren AST
         | End
         deriving (Show)

{-
expr :: [Tk] -> (AST, [Tk])
expr [] = error "Unexpected EOF while parsing"

expr (Tk TInt v : Tk TOp op : xs) =
  let e1 = Num (read v)
      (e2, xs') = term xs
  in  case op of
        "+" -> (Plus e1 e2, xs')
        "-" -> (Minus e1 e2, xs')

expr (Tk TOp "-" : xs) =
  let (e, xs') = expr xs
  in  (UMinus e, xs')

expr (Tk TInt v : xs) = (Num (read v), xs)
expr _ = error "Syntax error"

term :: [Tk] -> (AST, [Tk])
term [] = error "Unexpected EOF while parsing"
term (Tk TInt v : Tk TOp op : xs) =
  let e1 = Num (read v)
      (e2, xs') = factor xs
  in  case op of
        "*" -> (Mul e1 e2, xs')
        "/" -> (Div e1 e2, xs')

term (Tk TInt v : xs) = (Num (read v), xs)
term _ = error "Syntax error"

factor :: [Tk] -> (AST, [Tk])
factor [] = error "Unexpected EOF while parsing"
factor (Tk TInt v : xs) = (Num (read v), xs)
-}

{-
Grammar:
prog = expr
expr = NUM ((PLUS | MINUS) NUM)*

-}

peek :: [Tk] -> Maybe Tk
peek [] = Nothing
peek (x:_) = Just x

expr [] = (End, [])
expr xs = 
  let (e1, xs') = term xs
      op = value (head xs')
  in  case op of
        "+" -> let (e2, xs'') = expr (tail xs') in (Plus e1 e2, xs'')
        "-" -> let (e2, xs'') = expr (tail xs') in (Minus e1 e2, xs'')

term xs = 
  let (e1, xs') = factor xs
      op = value (head xs')
  in  case op of
        "*" -> let (e2, xs'') = term (tail xs') in (Plus e1 e2, xs'')
        "/" -> let (e2, xs'') = term (tail xs') in (Minus e1 e2, xs'')

factor [] = error "Unexpected EOF while parsing"
factor (Tk TInt v : xs) = (Num (read v), xs)

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

-- TESTING
ast = expr . lexer
calc = eval . fst . ast

test :: Bool
test =
  calc "1+1"   == 2.0 &&
  calc "1+2*3" == 7.0 &&
  calc "2*3+1" == 7.0

main :: IO ()
main = putStrLn "Hello, world!"
























