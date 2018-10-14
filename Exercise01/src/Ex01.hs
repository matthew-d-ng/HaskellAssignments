{- ngm1 Matthew Ng -}
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
evalOp d op x y
  = let  r = eval d x ; s = eval d y
  in case (r,s) of
    (Just m,Just n)  ->  Just (m `op` n)
    _                ->  Nothing

eval :: EDict -> Expr -> Maybe Double
eval d (Def x e1 e2)
    = case eval d e1 of
           Nothing -> Nothing
           Just v1 -> eval (define d x v1) e2

eval d (Add x y) = evalOp d (+) x y
eval d (Mul x y) = evalOp d (*) x y
eval d (Sub x y) = evalOp d (-) x y
eval d (Dvd x y)
        = case (eval d x, eval d y) of
               (Just m,Just n)
                  ->  if n==0.0 then Nothing else Just (m/n)
               _  ->  Nothing

eval d (Var i) = find d i
eval _ (Val a) = Just a

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
simpVar d v
  = let v' = eval d (Var v)
    in case v' of
        Just e     -> (Val e)
        Nothing    -> (Var v)

simpAdd :: EDict -> Expr -> Expr -> Expr
simpAdd _ e (Val 0.0)     = e
simpAdd _ (Val 0.0) e     = e
simpAdd d (Val e) (Val f) = case eval d (Add (Val e) (Val f)) of Just h -> (Val h)
simpAdd _ e1 e2           = (Add e1 e2)

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub _ e (Val 0.0)     = e
simpSub d (Val e) (Val f) = case eval d (Sub (Val e) (Val f)) of Just h -> (Val h)
simpSub _ e1 e2           = (Sub e1 e2)

simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d e (Val 1.0)     = e
simpMul d (Val 1.0) e     = e
simpMul d (Val e) (Val f) = case eval d (Mul (Val e) (Val f)) of Just h -> (Val h)
simpMul _ e1 e2           = (Mul e1 e2)

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d e (Val 0.0)     = (Dvd e (Val 0.0))
simpDvd d e (Val 1.0)     = e
simpDvd d (Val 0.0) e     = (Val 0.0)
simpDvd d (Val e) (Val f) = case eval d (Dvd (Val e) (Val f)) of Just h -> (Val h)
simpDvd _ e1 e2           = (Dvd e1 e2)

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v (Val e1) e2   = simp (define d v e1) e2
