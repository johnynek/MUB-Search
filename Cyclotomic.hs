
import Ratio
import Complex(Complex)


{-
  Type class definition for elements in a complex field.
-}
--class (Num a) => ComplexFieldEl a where
--  conj :: a -> a
--  inv  :: a -> a
--  mag2 :: a -> a
--  approx :: a -> Complex Float
--  realPart :: a -> a
--  imagPart :: a -> a


class (Num a) => ComplexRational a where
  conj :: a -> a
  inv  :: a -> a
  --mag2 :: a -> a
  approx :: a -> Complex Float

  boundRealOmega :: Integer -> a -> (Rational, Rational)
  boundImagOmega :: Integer -> a -> (Rational, Rational)
  boundReal :: Integer -> a -> (Rational, Rational)
  boundImag :: Integer -> a -> (Rational, Rational)
  boundMag2 :: Integer -> a -> (Rational, Rational)


{-
  A 'Cyclotome' is a member of a cyclotomic field.  It can be either a rational (a 'CycloRat') or
  a linear combination of members of a cyclotomic field (a 'CycloVal').
-}
data Cyclotome = CycloRat Rational
                  | CycloVal Cyclotome Cyclotome Cyclotome


{-
  Cyclotomes instantiate the 'ComplexRational' class.
-}
instance ComplexRational Cyclotome where
  conj (CycloRat x) = CycloRat x
  conj (CycloVal (CycloRat x) (CycloRat gamma) (CycloRat y)) = CycloVal (CycloRat x) (CycloRat gamma) (CycloRat (-y))
  conj (CycloVal x gamma y) = CycloVal (conj x) gamma (conj y)

  inv (CycloRat x) = CycloRat (1 / x)
  inv (CycloVal x gamma y) = CycloVal (x * (inv (x*x - y*y*gamma))) gamma ((-y) * (inv (x*x - y*y*gamma)))

  --mag2 (CycloRat x) = CycloRat ((abs x)^2)
  --mag2 (CycloVal x gamma y) = CycloVal ((mag2 x) + (mag2 y) + (y * x)) gamma (x * y * (inv gamma))

  approx (CycloRat x) = fromRational x
  approx (CycloVal x gamma y) = (approx x) + (sqrt $ approx gamma) * (approx y)


  boundMag2 k c = ((fst r)^2 + (fst i)^2, (snd r)^2 + (snd i)^2)
                  where r = boundReal k c
                        i = boundImag k c

  boundRealOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = (0, 0)
  boundRealOmega k (CycloVal x gamma y) = (l, u)
                                          where l = fst $ boundSqrt k $ (1 + (fst $ boundReal k gamma)) / 2
                                                u = snd $ boundSqrt k $ (1 + (snd $ boundReal k gamma)) / 2

  boundImagOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = (1, 1)
  boundImagOmega k (CycloVal x gamma y) = (l, u)
                                          where l = (fst $ boundImag k gamma) / (snd $ boundSqrt k (2 + 2 * (snd $ boundReal k gamma)))
                                                u = (snd $ boundImag k gamma) / (fst $ boundSqrt k (2 + 2 * (fst $ boundReal k gamma)))

  boundReal k (CycloRat x) = (x, x)
  boundReal k (CycloVal x gamma y) = (min, max)
                                     where a  = boundReal k x
                                           b  = boundReal k y
                                           c  = boundRealOmega k (CycloVal x gamma y)
                                           d  = boundImag k y
                                           e  = boundImagOmega k (CycloVal x gamma y)
                                           opts = [a, b, c, d, e]
                                           args = Prelude.map (allArgChoices opts) [0 .. (2^5) - 1]
                                           vals = Prelude.map (\ x -> (x !! 0)  + (x !! 1) * (x !! 2) - (x !! 3) * (x !! 4)) args
                                           min = minimum vals
                                           max = maximum vals


  boundImag k (CycloRat x) = (0, 0)
  boundImag k (CycloVal x gamma y) = (min, max)
                                     where a  = boundImag k x
                                           b  = boundImag k y
                                           c  = boundRealOmega k (CycloVal x gamma y)
                                           d  = boundReal k y
                                           e  = boundImagOmega k (CycloVal x gamma y)
                                           opts = [a, b, c, d, e]
                                           args = Prelude.map (allArgChoices opts) [0 .. (2^5) - 1]
                                           vals = Prelude.map (\ x -> (x !! 0)  + (x !! 1) * (x !! 2) + (x !! 3) * (x !! 4)) args
                                           min = minimum vals
                                           max = maximum vals


allArgChoices :: [(Rational, Rational)] -> Integer -> [Rational]
allArgChoices (h : t) n = if  null t
                          then [thisChoice]
                          else thisChoice : restChoices
                          where thisChoice = if   mod n 2 == 0
                                             then fst h
                                             else snd h
                                restChoices = allArgChoices t (div n 2)


{-
  Cyclotomes instantiate the 'Num' class, inherited through the 'ComplexFieldEl' class.
-}
instance Num Cyclotome where
  (CycloRat x0) + (CycloRat x1) = CycloRat (x0 + x1)
  (CycloVal x0 gamma0 y0) + (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 + x1) gamma0 (y0 + y1)
                                                      else error "Gamma mismatch"

  (CycloRat x0) - (CycloRat x1) = CycloRat (x0 - x1)
  (CycloVal x0 gamma0 y0) - (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 - x1) gamma0 (y0 - y1)
                                                      else error "Gamma mismatch"

  (CycloRat g) * (CycloVal x gamma y) = CycloVal ((CycloRat g) * x) gamma ((CycloRat g) * y)
  (CycloRat x0) * (CycloRat x1) = CycloRat (x0 * x1)
  (CycloVal x0 gamma0 y0) * (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 * x1 + y0 * y1 * gamma0) gamma0 (x0 * y1 + x1 * y0)
                                                      else error "Gamma mismatch"

  fromInteger n = CycloRat (fromInteger n)

  abs x = undefined
  signum x = undefined


{-
  Cyclotomes instantiate the 'Eq' class, inherited through the 'Num' class.
-}
instance Eq Cyclotome where
  (CycloRat x0) == (CycloRat x1) = x0 == x1
  (CycloVal x0 gamma0 y0) == (CycloVal x1 gamma1 y1) = (x0 == x1) && (gamma0 == gamma1) && (y0 == y1)


{-
  Cyclotomes instantiate the 'Show' class, inherited through the 'Num' class.
-}
instance Show Cyclotome where
  showsPrec v (CycloRat x) r = (showsPrec v (numerator x) "") ++ " / " ++ (showsPrec v (denominator x) "") ++ r
  showsPrec v (CycloVal x gamma y) r = "(" ++ (showsPrec v x "") ++ ", " ++ (showsPrec v gamma "") ++ ", " ++ (showsPrec v y "") ++ ")" ++ r


{-
  Construct a primitive (2^n)th root of unity.
-}
cycloGamma 0 = CycloRat 1
cycloGamma 1 = CycloRat (-1)
cycloGamma n = CycloVal (cycloZero (n - 1)) (cycloGamma (n - 1)) (cycloOne  (n - 1))


{-
  Construct zero.  TODO: explanation
-}
cycloZero  0 = CycloRat 0
cycloZero  1 = CycloRat 0
cycloZero  n = CycloVal (cycloZero (n - 1)) (cycloGamma (n - 1)) (cycloZero (n - 1))


{-
  Construct one.  TODO: explanation.
-}
cycloOne   0 = CycloRat 1
cycloOne   1 = CycloRat 1
cycloOne   n = CycloVal (cycloOne  (n - 1)) (cycloGamma (n - 1)) (cycloZero (n - 1))


{-
  Construct all the (2^n)th roots of unity.
-}
rootsOfUnity n = take (2^n) $ iterate (* (cycloGamma n)) (cycloGamma n)


{-
  Compute the kth iteration of converging upper and lower bounds on the square root
  of a rational number.
-}
boundSqrt :: Integer -> Rational -> (Rational, Rational)
boundSqrt 0 x = (head s, head g)
                where s = Prelude.filter ((< x) . (^2)) [x, (x-1)..]
                      g = Prelude.filter ((> x) . (^2)) [1..]
boundSqrt k 0 = (0, 0)
boundSqrt k x = if   (c^2) < x
                then (c, u)
                else (l, c)
                where p = boundSqrt (k - 1) x
                      l = fst p
                      u = snd p
                      c = (l + u) / 2

