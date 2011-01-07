{-
  An exact representation of cyclotomic fields
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  This module provides an exact representation of elements in a cyclotomic
  field - an extension field of the rationals that adjoins a primitive nth root
  of unity.  In this implementation, only cyclotomic fields with n = 2^m (where
  m is any natural number) are supported.
-}

module Cyclotomic (Cyclotome, approx, mag2, mag, real, imag, sqrtCR, boundSqrtS, cycloGamma, cycloZero, cycloOne, rootsOfUnity) where

import Bound
import Ratio
import Data.List(genericIndex)
import Complex(Complex)


{-
  Type definition for members of a cyclotomic field.

  The structure is a full ternary tree.  Leafs hold a rational value. Nodes
  hold no value; they are defined entirely by their children.

  A node has three children, which are read left to right as x, gamma, and y.
  A node represents the value x + sqrt(gamma) * y, where sqrt is the principle
  square root function.

  The height of a tree and the fact that it is full and ternary determines its
  shape.  The gammas attached to each node collectively determine its "color".
  Two trees with identical shape and color describe cyclotomes from the same
  cyclotomic field.

  Mathematically this is a sufficient, but not necessary, condition, but for
  the purposes of implementation it is required.  If two trees have different
  shape a "field mismatch" occurs.  If two trees have different color a "gamma
  mismatch" occurs.  Sometimes it's not possible to determine which mismatch
  really occurred.
-}
data Cyclotome = CycloRat Rational
                  | CycloVal Cyclotome Cyclotome Cyclotome



{-
  Cyclotomes instantiate the 'Num' class.
-}
instance Num Cyclotome where
  {-
    Addition is pointwise, with gammas left alone.  The trees must be the same
    height (and since they are necessarily full, the same shape).  If they are
    not the same height a "field mismatch" or a "color mismatch" occurs.

    The trees must have the same color.  If they are not the same color a
    "color mismatch" occurs.
  -}
  (CycloRat x0) + (CycloRat x1) = CycloRat (x0 + x1)
  (CycloRat x0) + (CycloVal x1 gamma1 y1) = error "Field mismatch"
  (CycloVal x0 gamma0 y0) + (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 + x1) gamma0 (y0 + y1)
                                                      else error "Color mismatch"


  {-
    Subtraction follows addition.
  -}
  (CycloRat x0) - (CycloRat x1) = CycloRat (x0 - x1)
  (CycloRat x0) - (CycloVal x1 gamma1 y1) = error "Field mismatch"
  (CycloVal x1 gamma1 y1) - (CycloRat x0) = error "Field mismatch"
  (CycloVal x0 gamma0 y0) - (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 - x1) gamma0 (y0 - y1)
                                                      else error "Color mismatch"


  {-
    Multiplication is complex (but not complicated!).  The trees must be the
    same height and color.  If they are not the same height and color a "color
    mismatch" occurs.

    The exception is multiplication of a leaf against a full tree, which is
    allowed.
  -}
  (CycloRat g) * (CycloVal x gamma y) = CycloVal ((CycloRat g) * x) gamma ((CycloRat g) * y)
  (CycloRat x0) * (CycloRat x1) = CycloRat (x0 * x1)
  (CycloVal x0 gamma0 y0) * (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 * x1 + y0 * y1 * gamma0) gamma0 (x0 * y1 + x1 * y0)
                                                      else error "Color mismatch"


  {-
    Constructing a cyclotome from an integer always produces a leaf.  To
    promote rationals into cyclotomic fields, multiply against a tree
    constructed with cycloOne.
  -}
  fromInteger n = CycloRat (fromInteger n)


  {-
    The absolute value of a complex number is undefined.
  -}
  abs x = undefined


  {-
    The sign of a complex number is undefined.
  -}
  signum x = undefined



{-
  Cyclotomes instantiate the 'Eq' class, inherited through the 'Num' class.

  Two cyclotomes are equivalent if they are exactly identical.  This doesn't
  account for multiple numerically-equivalent constructions, so these routines
  aren't to be used unless you pay attention to what you're doing and verify
  that you won't accidentally call two cyclotomes unequal that are simply
  constructed differently.
-}
instance Eq Cyclotome where
  (CycloRat x0) == (CycloRat x1) = x0 == x1
  (CycloVal x0 gamma0 y0) == (CycloVal x1 gamma1 y1) =    (x0 == x1)
                                                       && (gamma0 == gamma1)
                                                       && (y0 == y1)



{-
  Cyclotomes instantiate the 'Show' class, inherited through the 'Num' class.
-}
instance Show Cyclotome where
  showsPrec v (CycloRat x) r = (showsPrec v (numerator x) "") ++ " / " ++ (showsPrec v (denominator x) "") ++ r
  showsPrec v (CycloVal x gamma y) r = "(" ++ (showsPrec v x "") ++ ", " ++ (showsPrec v gamma "") ++ ", " ++ (showsPrec v y "") ++ ")" ++ r



{-
  Approximation of a complex number as a complex floating point number.

  This is fast and easy but not to be used other than visualization.
-}
approx (CycloRat x) = fromRational x
approx (CycloVal x gamma y) = (approx x) + (sqrt $ approx gamma) * (approx y)


{-
  kth iteration of converging upper and lower rational bounds on the magnitude
  squared of a cyclotome.
-}
mag2 :: Cyclotome -> ConvergingReal
mag2 c = (powCR r 2) + (powCR i 2) --make it clear we are squaring, not just multiplying, slight diff
                where r = real c
                      i = imag c
mag :: Cyclotome -> ConvergingReal
mag c = sqrtCR (mag2 c)
{-
  kth iteration of converging lower and upper rational bounds on the real part
  of the square root of gamma in a cyclotome.
  Re(\sqrt{a+ib}) = \sqrt{(r+a)/2}
  Im(\sqrt{a+ib}) = b/\sqrt{2(r+a)}
  if |r| = 1, we have the below:
-}
realSqrt :: Cyclotome -> ConvergingReal
realSqrt (CycloRat (-1)) = fromInteger 0 {- case of \Im( \sqrt{ -1 } ) -}
realSqrt gamma = sqrtCR ((one + a)/two)
                 where a = real gamma
                       one = fromInteger 1
                       two = fromInteger 2
{-
  kth iteration of converging lower and upper rational bounds on the imaginary
  part of the square root of gamma in a cyclotome.
  Im(\sqrt{a+ib}) = b/\sqrt{2(r+a)}
-}
imagSqrt :: Cyclotome -> ConvergingReal
imagSqrt (CycloRat (-1)) = fromInteger 1 {- case of \Im( \sqrt{ -1 } ) -}
imagSqrt gamma = b / (sqrtCR (two*(one + a)))
                   where b = imag gamma
                         a = real gamma
                         one = fromInteger 1
                         two = fromInteger 2
{-
  kth iteration of converging lower and upper bounds on the real part of
  a cyclotome.
  Re(a*b) = Re(a)Re(b) - Im(a)Im(b)
-}
real :: Cyclotome -> ConvergingReal
real (CycloRat x) = fromRational x {- Simple case of a real rational number -}
real (CycloVal x gamma y) = a + b*c - d*e
                                   where a  = real x
                                         b  = real y
                                         c  = realSqrt gamma 
                                         d  = imag y
                                         e  = imagSqrt gamma
{-
  kth iteration of converging lower and upper bounds on the imaginary part of
  a cyclotome.
  Im(a*b) = Re(a)Im(b) + Im(a)Re(b)
-}
imag (CycloRat x) = fromInteger 0 {- Simple case of a real rational-> no imag. -}
imag (CycloVal x gamma y) = a + b*c + d*e 
                                   where a  = imag x
                                         b  = imag y
                                         c  = realSqrt gamma
                                         d  = real y
                                         e  = imagSqrt gamma
{-
  A primitive (2^m)th root of unity in the cyclotomic field Q(Zeta_{2^m}).
-}
cycloGamma 0 = CycloRat 1
cycloGamma 1 = CycloRat (-1)
cycloGamma m = CycloVal (cycloZero (m - 1)) (cycloGamma (m - 1)) (cycloOne  (m - 1))

{-
  The number 0 in the cyclotomic field Q(Zeta_{2^m}).
-}
cycloZero  0 = CycloRat 0
cycloZero  1 = CycloRat 0
cycloZero  m = CycloVal (cycloZero (m - 1)) (cycloGamma (m - 1)) (cycloZero (m - 1))

{-
  The number 1 in the cyclotomic field Q(Zeta_{2^m}).
-}
cycloOne   0 = CycloRat 1
cycloOne   1 = CycloRat 1
cycloOne   m = CycloVal (cycloOne  (m - 1)) (cycloGamma (m - 1)) (cycloZero (m - 1))

{-
  The (2^m)th roots of unity.
  where w = e^{2\pi i/2^m}
  [ w^0, w^1, w^2, ... w^(2^m - 1) ]
-}
rootsOfUnity m = take (2^m) $ iterate (* (cycloGamma m)) (cycloOne m)


pow2 = iterate (\ x -> (1+(fst x), 2 * (snd x))) (0,1) :: [(Integer, Integer)]
-- if intLog2 r = L, then 2^L <= r < 2^(L+1)
intLog2 :: Integer -> Integer
intLog2 r = (fst $ head $ filter ((> r) . snd) pow2) - 1
{-
  intLogBase return the smallest L such that r < b^(L+1)
-}
intLogBase :: Integer -> Integer -> Integer
intLogBase b r = fst $ head $ filter ((> r) . snd) pows'
                 where pows' = genpow b 0 1 {- [(0,b), (1,b^2), ... (n,b^(n+1))...] -}
                       genpow b i acc = (i,nacc):genpow b (i+1) nacc
                                        where nacc = b * acc
{-
  kth iteration of converging upper and lower bounds on the square root of a
  rational number.
-}
bound0SqrtI :: Integer -> Bound Rational
bound0SqrtI 0 = Bound 0 0
bound0SqrtI 1 = Bound 1 1
bound0SqrtI x | x < 0 = error $ "Cannot take the sqrt of a negative number: " ++ (show x)
bound0SqrtI x = Bound low up
                where logx = intLog2 x
                      low = 2 ^ (logx `div` 2)
                      up = 2 * low
bound0Sqrt :: Rational -> Bound Rational
bound0Sqrt r = xb / yb
                 where xb = bound0SqrtI $ numerator r
                       yb = bound0SqrtI $ denominator r
bound0SqrtB :: Bound Rational -> Bound Rational
bound0SqrtB (Bound rl ru) = union (xbl / ybl) (xbu / ybu)
                 where xbl = bound0SqrtI $ numerator rl
                       ybl = bound0SqrtI $ denominator rl
                       xbu = bound0SqrtI $ numerator ru
                       ybu = bound0SqrtI $ denominator ru
{- refines a bound on the square-root of a rational -}
boundSqrt' :: Bound Rational -> Rational -> Bound Rational
boundSqrt' (Bound low up) r = next 
                          where low' = r/up
                                u0 = r/low -- always an upper
                                up' = (up + low')/2 {- always greater than sqrt if up is -}
                                u2 = (low + u0)/2 -- Also an upper bound
                                up'' = min up' u2
                                low'' = max low' low
                                mid = (low'' + up'')/2
                                next = case (compare (mid^2)  r) of
                                         EQ -> Bound mid mid
                                         LT -> Bound mid up''
                                         GT -> Bound low'' mid
boundSqrtBR :: Bound Rational -> Bound Rational -> Bound Rational
{-
  b0 : inital bound on the sqrt
  b1 : bound on the number in question
  assume: b0_lower <= sqrt(b1_lower) <= sqrt(b1_upper) <= b0_upper
  returns a better bound than b0. -}
boundSqrtBR (Bound low up) (Bound rl ru) = next
                          where low' = rl/up
                                low'' = max low' low
                                up' = (up + ru/up)/2 {- always greater than sqrt if up is -}
                                u2 = (low'' + ru/low'')/2 -- Also an upper bound
                                up'' = min up' u2
                                next = Bound low'' up''

boundSqrtS :: Rational -> [Bound Rational]
{- using this idea
fibs = scanl (+) 0 (1:fibs)
-}
boundSqrtS r = scanl boundSqrt' b0 (repeat r)
               where b0 = bound0Sqrt r

boundSqrt :: Integer -> Rational -> Bound Rational
boundSqrt k 0 = Bound 0 0
boundSqrt k 1 = Bound 1 1
{- here we accumulate a better and better bound tail recursively -}
boundSqrt k r = bs' b0 k r
                where b0 = bound0Sqrt r
                      bs' b 0 r = b
                      bs' b k r = bs' (boundSqrt' b r) (k-1) r
{-
boundSqrt 0 x = bound0Sqrt x
boundSqrt k x = case (compare  (c^2) x) of 
                  EQ -> Bound c c {- we hit it exactly -}
                  LT -> Bound c u'{- c^2 < x, so c is a lower bound -}
                  GT -> Bound l c {- c^2 > x, so c is upper bound -}
                where Bound l u = boundSqrt (k - 1) x
                      u' = (u + x/u)/2 {- always improve upper bound -}
                      c = (l + u') / 2 {- maybe improve the lower bound -}
-}

boundSqrtB :: Integer -> Bound Rational -> Bound Rational
boundSqrtB k (Bound l h) = Bound (lower (boundSqrt k l)) (upper (boundSqrt k h)) {- sqrt is monotonic -}

boundSqrtS' x = [boundSqrt k x | k <- [0..]]

-- This is an O(N^2) operation, not so great, avoid it...
-- We go a little deeper on the sqrt bound, because the first few aren't very useful
sqrtCR cr = ConvergingReal [boundSqrtB (k+2) b | (b,k) <- zip (toList cr) [0..]] 
-- sqrtCR cr = ConvergingReal $ drop 2 $ scanl boundSqrtBR (bound0SqrtB (head aslist)) aslist
--             where aslist = toList cr
