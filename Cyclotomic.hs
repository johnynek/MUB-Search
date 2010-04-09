{-
  An exact representation of cyclotomic fields
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  This module provides an exact representation of elements in a cyclotomic
  field - an extension field of the rationals that adjoins a primitive nth root
  of unity.  In this implementation, only cyclotomic fields with n = 2^m (where
  m is any natural number) are supported.
-}

module Cyclotomic (Cyclotome, approx, boundMag2, cycloGamma, cycloZero, cycloOne, rootsOfUnity) where

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
boundMag2 k c = r*r + i*i
                where r = boundReal k c
                      i = boundImag k c

{-
  kth iteration of converging lower and upper rational bounds on the real part
  of the square root of gamma in a cyclotome.
  Re(\sqrt{a+ib}) = \sqrt{(r+a)/2}
  Im(\sqrt{a+ib}) = b/\sqrt{2(r+a)}
  if |r| = 1, we have the below:
-}
boundRealOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = Bound 0 0 {- case of \Im( \sqrt{ -1 } ) -}
boundRealOmega k (CycloVal x gamma y) = boundSqrtB k ( (r+a)/two ) 
                                        where a = boundReal k gamma
                                              r = Bound 1 1
                                              two = Bound 2 2
{-
  kth iteration of converging lower and upper rational bounds on the imaginary
  part of the square root of gamma in a cyclotome.
  Im(\sqrt{a+ib}) = b/\sqrt{2(r+a)}
-}
boundImagOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = Bound 1 1 {- case of \Im( \sqrt{ -1 } ) -}
boundImagOmega k (CycloVal x gamma y) = b / (boundSqrtB k (two * (r+a))) 
                                        where b = boundImag k gamma
                                              a = boundReal k gamma
                                              r = Bound 1 1 {- |gamma| = 1 -} 
                                              two = Bound 2 2
{-
  kth iteration of converging lower and upper bounds on the real part of
  a cyclotome.
  Re(a*b) = Re(a)Re(b) - Im(a)Im(b)
-}
boundReal k (CycloRat x) = Bound x x {- Simple case of a real rational number -}
boundReal k (CycloVal x gamma y) = a + b*c - d*e
                                   where a  = boundReal k x
                                         b  = boundReal k y
                                         c  = boundRealOmega k (CycloVal x gamma y)
                                         d  = boundImag k y
                                         e  = boundImagOmega k (CycloVal x gamma y)
{-
  kth iteration of converging lower and upper bounds on the imaginary part of
  a cyclotome.
  Im(a*b) = Re(a)Im(b) + Im(a)Re(b)
-}
boundImag k (CycloRat x) = Bound 0 0 {- Simple case of a real rational-> no imag. -}
boundImag k (CycloVal x gamma y) = a + b*c + d*e 
                                   where a  = boundImag k x
                                         b  = boundImag k y
                                         c  = boundRealOmega k (CycloVal x gamma y)
                                         d  = boundReal k y
                                         e  = boundImagOmega k (CycloVal x gamma y)
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
-}
rootsOfUnity m = take (2^m) $ iterate (* (cycloGamma m)) (cycloGamma m)


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
bound0SqrtI x = Bound low up
                where logx = intLogBase 2 x
                      low = 2 ^ (logx `div` 2)
                      up = if even logx
                           then 2*low
                           else 2 ^ ((logx + 1) `div` 2) 
bound0Sqrt :: Rational -> Bound Rational
bound0Sqrt r = xb / yb
                 where xb = bound0SqrtI $ numerator r
                       yb = bound0SqrtI $ denominator r
{- refines a bound on the square-root of a rational -}
boundSqrt' :: Bound Rational -> Rational -> [Bound Rational]
boundSqrt' b@(Bound low up) r = b:boundSqrt' next r
                          where up' = (up + r/up)/2 {- always greater than sqrt if up is -}
                                mid = (low + up')/2
                                next = if mid^2 < r
                                       then Bound mid up'
                                       else Bound low mid
boundSqrtS :: Rational -> [Bound Rational]
boundSqrtS r = boundSqrt' b0 r
               where b0 = bound0Sqrt r

boundSqrt :: Integer -> Rational -> Bound Rational
{-
boundSqrt k r = genericIndex (boundSqrtS r) k
boundSqrt 0 x = (head s, head g)
                where s = Prelude.filter ((< x) . (^2)) [x, (x-1)..]
                      g = Prelude.filter ((> x) . (^2)) [1..]
-}
boundSqrt 0 x = bound0Sqrt x
boundSqrt k 0 = Bound 0 0
boundSqrt k x = case (compare  (c^2) x) of 
                  EQ -> Bound c c {- we hit it exactly -}
                  LT -> Bound c u'{- c^2 < x, so c is a lower bound -}
                  GT -> Bound l c {- c^2 > x, so c is upper bound -}
                where Bound l u = boundSqrt (k - 1) x
                      u' = (u + x/u)/2 {- always improve upper bound -}
                      c = (l + u') / 2 {- maybe improve the lower bound -}

boundSqrtB :: Integer -> Bound Rational -> Bound Rational
boundSqrtB k (Bound l h) = Bound (lower (boundSqrt k l)) (upper (boundSqrt k h)) {- sqrt is monotonic -}

boundSqrtS' x = [boundSqrt k x | k <- [0..]]
