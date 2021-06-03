module Main where

import Data.Char (isDigit, isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-----------
-- Lexer --
-----------

data TkType = TNum
            | TOp
            | TLparen
            | TRparen
            | TEof
            | TIdent
            deriving (Show)

data Tk = Tk { name  :: TkType 
             , value :: String }

instance Show Tk where
  show (Tk n v) = " "++show n++" "++v++"\n"

takeNum :: String -> String
takeNum [] = []
takeNum (x : '.' : xs) = 
  let ys = takeWhile isDigit xs in
  if not . null $ ys
    then x : '.' : ys
    else error "No digits after dot"
takeNum (x:xs) = 
  if isDigit x
    then x : takeNum xs
    else []


isOperator :: Char -> Bool
isOperator = flip elem ['+', '-', '*', '/']

scan :: String -> [Tk]
scan "" = [Tk TEof ""]
scan (x:xs)
  | isDigit x    = let ys = takeNum (x:xs) in Tk TNum ys : scan (drop (length ys - 1) xs)
  | x == ' '     = scan xs
  | isOperator x = Tk TOp [x] : scan xs
  | x == '('     = Tk TLparen [x] : scan xs
  | x == ')'     = Tk TRparen [x] : scan xs
  | isAlpha x    = let ys = x : takeWhile isAlphaNum xs in Tk TIdent ys : scan (drop (length ys - 1) xs)
  | otherwise    = error $ "Unknown symbol: '" ++ [x] ++ "'"

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
         | FnCall String AST -- name, args
         deriving (Show)

{-
  Grammar:
    program = expr
    expr = term ((PLUS | MINUS) term)*
    term = factor ((MUL | DIV) factor)*
    factor = MINUS NUM | NUM | LPAREN expr RPAREN | fn_call
    fn_call = IDENT LPAREN expr RPAREN
-}

expr, term, factor :: [Tk] -> (AST, [Tk])
expr [Tk TEof ""] = error "Unexpected EOF while parsing"
expr xs = 
  let (e1, xs') = term xs
      op = value (head xs')
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
factor (Tk TNum v : xs) = (Num (read v), xs)
factor (Tk TOp "-" : xs) = 
  let (e, xs') = factor xs in (UMinus e, xs')
factor (Tk TLparen _ : xs) =
  let (e, Tk t _ : xs') = expr xs
  in  case t of
        TRparen -> (e, xs')
        _       -> error "Unclosed parenthesis"
factor (Tk TIdent i : Tk TLparen _ : xs) = 
  let (e, Tk t _ : xs') = expr xs
  in  case t of
        TRparen -> (FnCall i e, xs')
        _       -> error $ "Unclosed parenthesis in function: '" ++ i ++ "'"
factor _ = error "Syntax error"
        
-----------------
-- Interpreter --
-----------------

-- List of built-in functions
fns :: [(String, Float -> Float)]
fns = [ ("sin", sin)
      , ("cos", cos)
      , ("tan", tan)
      , ("sinh", sinh)
      , ("cosh", cosh)
      , ("tanh", tanh)
      , ("log", log) ]

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
eval (FnCall i arg) =
  let a = eval arg
  in  findFn i a

findFn :: [Char] -> Float -> Float
findFn x = 
  let m = lookup x fns
  in  fromMaybe (error $ "Function not found: '" ++ x ++ "'") m

----------
-- Main --
----------
calc :: String -> Float
calc = eval . fst . expr . scan

removeQuotes :: String -> String
removeQuotes = filter (/= '"')

usage :: IO ()
usage = putStrLn "Usage: calc \"<expression>\""

main :: IO ()
main = do
  a <- getArgs
  if null a 
    then usage 
    else putStrLn . show . calc . removeQuotes . show . head $ a

