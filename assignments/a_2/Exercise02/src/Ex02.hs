{- butrfeld Andrew Butterfield -}
module Ex02 where
import Data.List ((\\))
--import Data.Maybe
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

v42 = Val 42 ; j42 = Just v42

-- Part 1 : Evaluating Expressions -- (60 test marks, worth 15 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

evalOp d op x y = 
	let r = eval d x ; s = eval d y in
	case (r,s) of
		(Just m,Just n) -> Just (m `op` n)
		_				-> Nothing

eval :: EDict -> Expr -> Maybe Double

eval _ (Val x) = Just x
eval d (Var i) = find d i


		 
		 
eval d (Add x y) = evalOp d (+) x y
eval d (Mul x y) = evalOp d (*) x y
eval d (Sub x y) = evalOp d (-) x y

	
eval d (Dvd x y) = 
	case (eval d x, eval d y) of	
	(Just m, Just n) -> if n == 0.0 then Nothing else Just (m-n)
	_                -> Nothing
	

	
eval d (Def x e1 e2) =
	case eval d e1 of
		Nothing -> Nothing
		Just e1 -> eval (define d x e1) e2
	


-- Part 1 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y            =  y + z         Law 1 
  x + (y + z)      =  (x + y) + z   Law 2
  x - (y + z)      =  (x - y) - z   Law 3
  (x + y)*(x - y)  =  x*x - y*y     Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
law1 e = j42

law2 :: Expr -> Maybe Expr
law2 e = j42

law3 :: Expr -> Maybe Expr
law3 e = j42

law4 :: Expr -> Maybe Expr
law4 e = j42
