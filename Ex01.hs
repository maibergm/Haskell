{- 14317323 - Maksim Maiberg -}
module Ex01 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

-- Part 1 : Evaluating Expressions -- (63 marks) -------------------------------

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.
  
eval :: EDict -> Expr -> Maybe Double
eval [] (Var x) = Nothing
eval [] (Val x) = Just x
eval d (Var x) = find d x
eval d (Val x) = Just x
eval d (Add x y) = let r = eval d x ; s = eval d y 
				in case (r,s) of 
					(Just m, Just n) -> Just (m+n)
					(_) -> Nothing 
eval d (Sub x y) = let r = eval d x ; s = eval d y 
				in case (r,s) of 
					(Just m, Just n) -> Just (m-n)
					(_) -> Nothing
eval d (Mul x y) = let r = eval d x ; s = eval d y 
				in case (r,s) of 
					(Just m, Just n) -> Just (m*n)
					(_) -> Nothing
eval d (Dvd x y) = let r = eval d x ; s = eval d y 
				in case (r,s) of 
					(Just m, Just n) -> if n == 0.0 then Nothing else Just (m/n)
					(_) -> Nothing
eval d e = Nothing
-- Part 2 : Simplifying Expressions -- (57 marks) ------------------------------

-- Given the following code :

simp :: EDict -> Expr -> Expr
simp d (Var v)        =  simpVar d v
simp d (Add e1 e2)    =  simpAdd d   (simp d e1) (simp d e2)
simp d (Sub e1 e2)    =  simpSub d   (simp d e1) (simp d e2)
simp d (Mul e1 e2)    =  simpMul d   (simp d e1) (simp d e2)
simp d (Dvd e1 e2)    =  simpDvd d   (simp d e1) (simp d e2)
simp d (Def v e1 e2)  =  simpDef d v (simp d e1) (simp d e2)
simp _ e = e  -- simplest case, Val, needs no special treatment

-- Implement the following functions so all 'simp' tests pass.

  -- (1) see test scripts for most required properties
  -- (2) (Def v e1 e2) should simplify to just e2 in the following two cases:
    -- (2a) if there is mention of v in e2
    -- (2b) if any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr
simpVar d e = case (find d e) of 
					(Just a) -> (Val a)
					(_) -> (Var e)
simpVar d v = (Val 1e-99)

simpAdd :: EDict -> Expr -> Expr -> Expr
simpAdd d x y = case (x,y) of 
					(e, Val 0.0) -> e 
					(Val 0.0, e) -> e 
					(Val a, Val b) -> Val (a + b)
simpAdd d e1 e2 = (Val 1e-99)

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub d x y = case (x,y) of 
					(e, Val 0.0) -> e 
					(Val 0.0, e) -> e 
					(Val a, Val b) -> Val (a - b)
simpSub d e1 e2 = (Val 1e-99)

simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d x y = case (x,y) of 
					(e, Val 0.0) -> e 
					(Val 0.0, e) -> e 
					(Val a, Val b) -> Val (a * b)
simpMul d e1 e2 = (Val 1e-99)

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d x y = case (x,y) of 
					(e, Val 0.0) -> e 
					(Val 0.0, e) -> e 
					(Val a, Val b) -> if b == 0.0 then Val a else Val (a/b)
simpDvd d e1 e2 = (Val 1e-99)

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d x y z = if simpVar d x == z then y else z
simpDef d v e1 e2 = (Val 1e-99)
