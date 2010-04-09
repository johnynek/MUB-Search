{-
  Here we make a type to deal with the math of bounds.  This is so we can 
  use the bounds in the normal formulas and not continually calculate them
  by hand
-}

module Bound (Bound(..), boundContains, toTuple) where

data (Ord a) => Bound a = Bound { lower :: a, upper :: a } | Unbounded
             deriving (Show, Eq)

boundContains :: (Ord a) => Bound a -> a -> Bool
boundContains Unbounded _ = True
boundContains (Bound x y) z = (x <= z) && (z <= y)
toTuple (Bound x y) = (x, y)

instance (Ord a, Num a) => Num (Bound a) where
  signum _ = undefined
  fromInteger x = Bound bx bx
                  where bx = fromInteger x
  abs b@(Bound l h) = if l < 0
                      then Bound 0 (max (abs l) (abs h))
                      else b {- u >= l, so, in this case, both are positive -}
  {- Here's how to add bounds -} 
  {- Bound with everything -}
  (Bound x y) + (Bound z w) = Bound (x+z) (y+w)
  Unbounded + _ = Unbounded 
  _ + Unbounded = Unbounded 
  {- negation switches high and low -} 
  negate (Bound x y) = Bound (negate y) (negate x)
  negate Unbounded = Unbounded

  {- Here's how to multiply bounds -}
  {- Bound with everything -}
  (Bound x y) * (Bound z w) = Bound (minimum all) (maximum all)
                              where all = [ x*z, x*w, y*z, y*w ]
  Unbounded * _ = Unbounded 
  _ * Unbounded = Unbounded 
   
instance (Ord a, Fractional a) => Fractional (Bound a) where
  fromRational r = Bound fr fr
                   where fr = fromRational r
  recip Unbounded = Unbounded
  recip b@(Bound x y) = if boundContains b (fromInteger 0)
                        then Unbounded {- we can't deal with disjoint bounds -}
                        else Bound (recip y) (recip x)
