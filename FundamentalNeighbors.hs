{-
  Given d and n, find all vectors of the form [1, x_0, x_1, ... x_{d - 2}],
  where x_i is a 2^nth root of unity, that are orthogonal or unbiased to the
  vector [1, 1, ... 1].

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  Output omits the leading 1 in each vector.

  Usage:
    FundamentalNeighbors <d> <p> <e> <m> <s> <j>

    d: dimension of vectors to find.
    p: elements of vectors are 2^pth roots of unity.
    e: rational approximation threshold.  Use 0%1 for exact search.
    m: 0 -> find vectors orthogonal to [1, 1, ... 1].
       1 -> find vectors unbiased to [1, 1, ... 1].
    s: search is split into jobs of size s (use s = -1 to see the bounds on s).
    j: job index (use j = -1 to see the bounds on j).
-}

import System(getArgs)
import Ratio
import Data.List
import Data.Maybe

import SublistPred
import Cyclotomic
import Perms
import Bound


{-
  Sum of the elements in a list.

  This function differs from Haskell's sum function in that it does not assume
  that the type of the list elements includes a working fromInteger definition.

  While the Num class requires a definition of fromInteger, the Cyclotomic type
  only provides a stub and so we avoid using fromInteger here.
-}
rsum :: (Num a) => [a] -> a
rsum (h : t) = foldl (+) h t


{-
  Unique elements in a list.

  For a while I used this function instead of "nub" in lunions, below.  I
  believe this was before I added support for distributing this program (see
  the usage instructions) and so this can probably be removed now.  There
  doesn't seem to be any major speed differences between unique and nub,
  and since nub is in the Prelude we should stick with that.
-}
{-unique xs =
 let
  r = u xs 0                         -- result list
  u []     _ = []                    -- build result
  u (x:xs) n = if member x r n then u xs n
               else x:(u xs (n+1))
  member e xs     0 = False
  member y (x:xs) n = x==y || member y xs (n-1)
 in r
-}


{-
  Union of all lists in a list.
-}
lunions :: (Eq a) => [[a]] -> [a]
lunions []       = []
{- 
lunions (h : t)  = Data.List.union (nub h) (lunions t)
-}
lunions x = nub (concat x)


{-
  Predicate that determines if a vector of 2^pth roots of unity is orthogonal
  to the vector [1, 1, ... 1].

  First, we construct the inner product between the provided vector (including
  the omitted element 1) and the vector [1, 1, ... 1].  This means summing up
  the provided vector, along with 1.  s defines this value (a Cyclotome).

  Second, we construct an iteration of tightening bounds on the magnitude-
  squared of s.  The "tightness" of the bound increases by 4 with each
  iteration.  a defines these bounds (an infinite list of tuples).

  Third, we filter this list to only include those bounds that are tight enough
  to definitively decide ||x||^2 < e; that is, those whose lower bound is
  greater than e and those whose upper bound is less than or equal to e.  t
  defines these bounds (an infinite list of tuples).

  We decide ||x|| <= e using the first of these vetted bounds.
-}
pOrth :: Rational -> [Cyclotome] -> Bool
pOrth e x = (upper $ head t) <= e2 
              where a = [boundMag2 k (rsum x) | k <- [1 ..]]
                    {- t is a list of bounds that clearly distinguish |x|^2 and e^2 -}
                    t = filter (\ b -> (not (boundContains b e2)) || ((upper b) == e2)) a
                    e2 = e*e
{-
  Predicate that determines if a vector is unbiased to the unity vector.

  | ||x|| - b | <= e.
-}
pBias :: [Bound Rational] -> Rational -> [Cyclotome] -> Bool
pBias b e x = (upper $ head t) <= e
              where s = rsum x
                    a = [abs((boundMag k s) - bk) | (k,bk) <- (zip [1 ..] b)]
                    {- t is a list of bounds that clearly distinguish ||x|^2-d| and e -}
                    t = filter (\ bnd -> (not (boundContains bnd e)) || ((upper bnd) == e)) a


{-
  Main entry point.
-}
main = do
  {-
    Command line arguments.
  -}
  args <- getArgs 
  let check = if (length args) < 5
              then error "usage: <d (dim)> <p (2^{th} roots)> <m (0=orth, 1=unbias)> <s (size)> <j (job)>"
              else return ()
  check
  let sD : sP : sM : sS : sJ : _ = args
  let d = read sD :: Integer
  let p = read sP :: Integer
  let m = read sM :: Integer
  let s = read sS :: Integer
  let j = read sJ :: Integer
  let n = 2^p

  {-
    We know the error on inner product is at most:
    2d\sin (2\pi / 2^(p+1)) = 2*d * Im( e^{2\pi i/2^{p+1}})
    If we are less than or equal to the lower bound, then we are less
    than or equal to the actual value
   -}
  {- going to the 5th iteration seems enough accurate enough -}
  let err_it = 5
  let err = (fromInteger (2*d)) * (lower (boundImag err_it (cycloGamma (p+1))))
  -- Pop the first several off so we the bound is more refined by the time we use it
  let sqrt_d = tails (boundSqrtS (fromInteger d)) !! (fromInteger err_it)
  {-
    The 2^pth roots of unity.
  -}
  let roots = rootsOfUnity p


  {-
    Job size of 0 implies entire job.
  -}
  let maxSize = (sublistCount (d - 1) roots) - 1
  let maxJobs = (sublistJobs (d - 1) roots s) - 1


  {-
    Convert a list of roots of unity to a list of indices in the list of all roots of unity.
  -}
  let lookup = map (\ x -> toInteger $ fromJust (findIndex (x ==) roots))


  {-
    All vectors of roots of unity that are orthogonal / unbiased to the unity vector, for
    the selected job.
  -}
  let isOrth v = pOrth err ((cycloOne p):v) 
  let isUnbiased v = pBias sqrt_d err ((cycloOne p):v) 

  let rootsOrth = genericIndex (sublistPredP isOrth (d - 1) roots s) j
  let rootsBias = genericIndex (sublistPredP isUnbiased (d - 1) roots s) j

  {-
    Select the desired list of vectors and form all permutations.
  -}
  let roots = if   m == 0
              then rootsOrth
              else rootsBias
  let vecs = map lookup roots
  let allVecs = lunions $ map permuteAllL vecs


  {-
    Output the list.
  -}
  let o | s == (-1) = [putStrLn ("1 <= s <= " ++ (show $ maxSize))]
        | (s < 1) || (s > maxSize) = [putStrLn ("Out of range.  1 <= s <= " ++ (show $ maxSize))]
        | j == (-1) = [putStrLn ("0 <= j <= " ++ (show $ maxJobs))]
        | (j < 0) || (j > maxJobs) = [putStrLn ("Out of range.  0 <= j <= " ++ (show $ maxJobs))]
        | otherwise = map (putStrLn . show) allVecs
  sequence_ o

