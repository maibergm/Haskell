{- butrfeld Andrew Butterfield -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype
data Tree k d
  = Br (Tree k d) (Tree k d) k d
  | Leaf k d
  | Nil
  deriving (Eq, Show)

type IntFun = Tree Int Int -- binary tree with integer keys and data

data Expr
  = Val Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Abs Expr
  | Sign Expr
   deriving (Eq, Show)



-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> Tree k d -> Tree k d
ins x y (Nil) = Leaf x y
ins x y (Leaf k d) 
	| x < k = (Br (Leaf x y) Nil k d) 
	| x > k = (Br Nil (Leaf x y) k d)
	| x == k = Leaf x y
ins x y (Br (a) (b) k d) 
	| x < k = (Br (ins x y a) (b) k d)
	| x > k = (Br (a) (ins x y b) k d)
	| x == k = (Br (a) (b) x y) 

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d
lkp (Nil) key =  fail ( "mfind")
lkp (Leaf k d) key 
	| key == k = return d
	| otherwise = fail ( "mfind")
lkp (Br (a) (b) k d) key 
	| key == k = return d
	| key < k = lkp a key 
	| key > k = lkp b key
	| otherwise = fail ( "mfind")

-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}

instance Num Expr where
  Val e1 + Val e2 = Val (e1 + e2)
  e1 + e2 = Add e1 e2
  Val x - Val y = Val (x - y)
  e1 - e2 = Sub e1 e2
  Val x * Val y = Val (x*y)
  e1 * e2 = Mul e1 e2
  negate (Val x) = Val( x - (x*2))
  negate e = Sub (Val 0.0) e
  abs (Val e) 
	| e >= 0 = (Val e) 
	| otherwise = (Val (e * (-1)))
  abs e = Abs e
  signum (Val e) 
	| e == 0 = (Val e)
	| e > 0 = (Val 1)
	| otherwise = (Val (-1))
  signum e = Sign e
  fromInteger i = Val (fromInteger i)	

