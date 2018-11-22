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
ins k d (Br left right bk bd)
        | k < bk              =   Br (ins k d left) right bk bd
        | k > bk              =   Br left (ins k d right) bk bd
        | otherwise           =   Br left right k d

ins k d (Leaf lk ld)
        | k < lk              =   Br (Leaf k d) (Nil) lk ld
        | k > lk              =   Br (Nil) (Leaf k d) lk ld
        | otherwise           =   Leaf k d

ins k d Nil                   =   Leaf k d

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d
lkp (Br left right bk d) k
        | k < bk              =   lkp left k
        | k > bk              =   lkp right k
        | otherwise           =   return d

lkp (Leaf lk d) k
        | k == lk             =   return d
        | otherwise           =   fail "Value not found"

lkp Nil _                     =   fail "Value not found"

-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}

instance Num Expr where
  Val v1 + Val v2   =   Val (v1 + v2)
  e1 + e2           =   Add e1 e2

  Val v1 - Val v2   =   Val (v1 - v2)
  e1 - e2           =   Sub e1 e2

  Val v1 * Val v2   =   Val (v1 * v2)
  e1 * e2           =   Mul e1 e2

  negate (Val v)    =   Val (v * (-1.0))
  negate e          =   Sub 0 e

  abs (Val v)       =   if v > 0 then (Val v) 
                        else Val (v * (-1.0))
  abs e             =   Abs e

  signum (Val v)    =   if v > 0 then (Val 1) 
                        else if v < 0 then (Val (-1.0))
                        else (Val 0.0)
  signum e          =   Sign e

  fromInteger i     =   Val (fromInteger i)
