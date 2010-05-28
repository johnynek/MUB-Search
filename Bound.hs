{-
  Here we make a type to deal with the math of bounds.  This is so we can 
  use the bounds in the normal formulas and not continually calculate them
  by hand
-}
module Bound (Bound(..), ConvergingReal(..), boundPlus, boundTimes, boundContains, boundLT, boundGT) where

import Ratio

data (Ord a) => Bound a = Bound { lower :: a, upper :: a } | Unbounded
             deriving (Show, Eq)

boundContains :: (Ord a) => Bound a -> a -> Bool
boundContains (Bound x y) z = (x <= z) && (z <= y)
boundContains Unbounded _ = True

boundLT :: (Ord a) => Bound a -> Bound a -> Bool
boundLT (Bound x y) (Bound z w) = y < z
boundLT Unbounded _ = False
boundLT _ Unbounded = False

boundGT :: (Ord a) => Bound a -> Bound a -> Bool
boundGT x y = boundLT y x

boundOverlap x y = not ((boundLT x y) || (boundGT x y))

boundPlus (Bound x y) z = Bound (x+z) (y+z)
boundPlus Unbounded _ = Unbounded

boundTimes (Bound x y) z | z >= (fromInteger 0) = Bound (x*z) (y*z)
                         | otherwise = Bound (y*z) (x*z)
boundTimes Unbounded _ = Unbounded

boundApprox :: (Ord a, Fractional a) => Bound a -> a
boundApprox b = ((lower b) + (upper b)) * fromRational(1%2)

instance (Ord a, Num a) => Num (Bound a) where
  signum _ = undefined
  fromInteger x = Bound bx bx
                  where bx = fromInteger x
  abs b@(Bound l h) = if l < 0
                      then if h >= 0
                           {- contain zero -}
                           then Bound 0 (max (abs l) (abs h))
                           {- reflect -}
                           else Bound (abs h) (abs l) 
                      else b {- u >= l, so, in this case, both are non-negative -}
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
  (Bound x y) * (Bound z w) | x >= 0 && z >= 0 = Bound (x*z) (y*w) {- lower bounds are positive -}
{-
                            | x == y = if x >= (fromInteger 0)
                                       then Bound (x*z) (x*w) {- common case -}
                                       else Bound (x*w) (x*z)
                            | z == w = if z >= (fromInteger 0)
                                       then Bound (x*z) (y*z) {- common case -}
                                       else Bound (y*z) (x*z)
-}
                            | otherwise = Bound (minimum all) (maximum all)
                                          where all = [ x*z, x*w, y*z, y*w ]
{- Simple version tends to be a bit slow 
  (Bound x y) * (Bound z w) = Bound (minimum all) (maximum all)
                              where all = [ x*z, x*w, y*z, y*w ]
-}
  Unbounded * _ = Unbounded 
  _ * Unbounded = Unbounded 
   
instance (Ord a, Fractional a) => Fractional (Bound a) where
  fromRational r = Bound fr fr
                   where fr = fromRational r
  recip Unbounded = Unbounded
  recip b@(Bound x y) = if boundContains b (fromInteger 0)
                        then Unbounded {- we can't deal with disjoint bounds -}
                        else Bound (recip y) (recip x)

-- a sequence of converging bounds
data ConvergingReal = ConvergingReal { toList :: [Bound Rational] }

instance Show ConvergingReal where
  show cr0 = show dec
           where dec = fromRational (boundApprox ((toList cr0) !! 3)) :: Double

instance Eq ConvergingReal where
  -- To be equal, they all overlap, this will take infinitely long
  cr0 == cr1 = if (b0 == b1) && (fromRational (lower b0) == b0) -- This is a rational number
               then True
               else (boundOverlap b0 b1) && ((ConvergingReal t0) == (ConvergingReal t1)) 
             where b0:t0 = toList cr0
                   b1:t1 = toList cr1

instance Ord ConvergingReal where
  -- <, <=, >, >=, max, min
  cr0 < cr1 = if not (bLT || bGT) 
              then (ConvergingReal t0) < (ConvergingReal t1)
              else bLT 
            where b0:t0 = toList cr0
                  b1:t1 = toList cr1
                  bLT = boundLT b0 b1
                  bGT = boundGT b0 b1

  -- make sure we avoid == if possible:
  cr0 <= cr1 = (cr0 < cr1) || (cr0 == cr1)
  cr0 > cr1 = (cr1 < cr0)
  cr0 >= cr1 = (cr1 < cr0) || (cr0 == cr1)
  max cr0 cr1 = if cr0 > cr1
                then cr0
                else cr1
  min cr0 cr1 = if cr0 < cr1
                then cr0
                else cr1
  
instance Num ConvergingReal where
  signum cr = if sl == sh
              then ConvergingReal (repeat (fromRational sh))
              else signum (ConvergingReal t)
             where
                   (Bound l h):t = toList cr
                   sl = signum l
                   sh = signum h
  fromInteger x = ConvergingReal (repeat (Bound rx rx))
                  where rx = fromInteger x
  -- lift the abs and negate functions:
  abs cr = ConvergingReal (map abs (toList cr))
  negate cr = ConvergingReal (map negate (toList cr))
  cr0 + cr1 = ConvergingReal [ b0 + b1 | (b0, b1) <- zip (toList cr0) (toList cr1)]
  cr0 * cr1 = ConvergingReal [ b0 * b1 | (b0, b1) <- zip (toList cr0) (toList cr1)]

instance Fractional ConvergingReal where
  fromRational r = ConvergingReal (repeat (Bound r r))
  recip cr = ConvergingReal (map recip (toList cr))
