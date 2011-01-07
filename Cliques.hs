{-
  Clique Finding on Vector Space Graphs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  TODO

  Note: the "Flexible Instances" and "Flexible Constructs" Haskell extensions
  are required.  In GHC/GHCI this necessitates the -XFlexibleInstances and
  -XFlexibleConstructs flags.
-}

module Cliques (toIntBE, fromIntBE, cliques, setToArray, isnbr_a) where

import Data.List
import Array
import qualified Data.Set as Set

{-
  Type class for scalars in the vector space underlying a vector space graph.

  e: equivalence predicate
  x: division
-}
class (Eq a, Ord a) => Scalar a where
  e  :: a -> a -> Bool
  x  :: Integer -> a -> a -> a


{-
  A vector is a list of scalars.
-}
type Vector a = [a]


{-
  A clique is a list of vectors.

  Most lists of vectors are not cliques, of course, but cliques are typed as
  lists of vectors.
-}
type Clique a = [Vector a]


{-
  Scalar field: any finite subset of consecutive integers.

  Equivalence: exact equality.
  Division:    subtraction modulo the size of the scalar field.
-}
instance Scalar Integer where
  e       = (==)
  x n x y = mod (x - y) n


{-
  Scalar field: fixed-length lists of integers over any finite subset of
  consecutive integers.

  Equivalence: exact equality.
  Division:    pointwise subtraction modulo the size of the underlying integer field.
-}
instance (Scalar [Integer]) where
  e       = (==)
  x n x y = zipWith (\ x y -> mod (x - y) n) x y


{-
  Scalar field: fixed-length lists of fixed-length lists over any finite subset
  of consecutive integers.

  Equivalence: permutations.
  Division:    undefined.
-}
instance (Scalar [[Integer]]) where
  e   x y = (sort x) == (sort y)
  x n x y = undefined


{-
  The intersection of all lists in a list.

  This is a reasonably fast implementation, which takes the pairwise
  intersections repeatedly until finished.

  Ex:  [ [1, 2, 6], [2, 6, 7], [2, 5, 6], [2, 5, 6], [2, 6, 8] ]
      ->  [ [2, 6], [2, 5, 6], [2, 6, 8] ]
      ->  [ [2, 6], [2, 6, 8] ]
      ->  [ [2, 6] ]
      ->  [2, 6]
ints :: (Scalar a) => [[a]] -> [a]
ints []          = []
ints (h : [])    = h
ints (h : g : t) = ints ((intersectBy e h g) : t)
-}

{- this won't work for order 2 scalars until they are instances of ord -}
ints :: (Ord a) => [[a]] -> [a]
ints [] = []
ints l = Set.toAscList( foldl1' Set.intersection (map (Set.fromList) l))

{-
  All size k super-cliques of a clique c, given a list r of potential
  extending vertices.

  Extending vertices are vertices that have been vetted as being neighbors to
  all vertices in c.  That is, any vertex in the list of extending vertices can
  be added to c to increase the size of the clique by one.

  The idea is to form all the extended cliques (all cliques formed by adding
  one of the extending vertices to c) and then find the lists of extending
  vertices for each such extended clique, and recurse until k = 0, collecting
  everything as we go.

  n is the size of the underlying scalar field.
  l is the list of adjacencies (see nbrs).
cliques' _ 0 _ (c, _) = [c]
cliques' n k l (c, r) = concatMap (cliques' n (k - 1) l) s
                        where s = map (\ v -> (v : c, intersectBy e r (nbrs n l v))) r
cliques' :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [Vector a] -> (Clique a, Set.Set (Vector a)) -> [Clique a]
cliques' n k l = cliques_g' (nbrs n l) k

{-cliques' n k l = cliques_g' (nbrs n l) k Try to make this more generic -}
cliques_g' :: (Scalar a, Scalar (Vector a)) => (Vector a -> Set.Set (Vector a)) -> Integer -> (Clique a, Set.Set (Vector a)) -> [Clique a]
cliques_g' _ 0 (c, _) = [c]
cliques_g' adj k (c, r) = concatMap (cliques_g' adj (k - 1)) s
                        {- old docs say the bigger set should be first in intersection -}
                        where s = map (\ v -> (v : c, Set.intersection (adj v) r)) $ Set.toAscList r
-}


{-
  All size k super-cliques of at least one clique from list cs.

  Essentially we are just wrapping cliques' for each clique in cs.

  n is the size of the underlying scalar field.
  l is the list of adjacencies (see nbrs).
cliques :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [Vector a] -> [Clique a] -> [Clique a]
cliques n k l cs = concatMap (\ c -> cliques' n (k' c) l (g c)) cs
                    where g  c = (c, ints (map (nbrs n l) c))
                          k' c = k - (toInteger . length) c

cliques :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [Vector a] -> Clique a -> [Clique a]
-}
{- cliques n k l= cliques_g (nbrs n l) k -}
cliques n k l cl = map snd $ ((iterate (cliques_g adj) [(ext,cl)] )) `genericIndex` k'
                      where k' = k - (genericLength cl)
                            cliques_g :: (a -> a -> Bool) -> [([a], [a])] -> [([a],[a])] 
                            cliques_g f cls = concatMap (cliqueExt f) cls
                            n_set = Set.fromList l
{-
                            adj = isnbr n n_set
-}
                            n_a = setToArray n n_set
                            adj = isnbr_a n n_a
                            ext = ints (map (nbrs n l) cl) 

{-
cliques_g :: (Scalar a, Scalar (Vector a)) => (Vector a -> Set.Set (Vector a)) -> Integer -> Clique a -> [Clique a]
cliques_g adj k c = cliques_g' adj k' (c, exts) 
                    where exts = foldl1' Set.intersection (map adj c) {- Set of all nodes LARGER and connected to c -}
                          k'= k - (toInteger . length) c
-}

{- yet another approach to clique finding, same basic algorithm, trying to be faster -}

{- Given an adjaceny function, and a tuple of (possible extensions, clique), return all children extensions to
   the clique.  Constrain: possibly extensions MUST be ordered lowest to highest -}
cliqueExt :: (a -> a -> Bool) -> ([a], [a]) -> [([a],[a])]
cliqueExt _ ([], clique) = [] {- unextendable -}
cliqueExt adj (h:t, clique) = (h_adj, h:clique) : (cliqueExt adj (t, clique))
                             where h_adj = filter (adj h) t
{-
isClique :: (a -> a -> Bool) -> [a] -> Bool
isClique _ [] =  True
isClique _ [h] = True
isClique adj h:t = (and (map (adj h) t)) && (isClique adj t)
-}

{-
  Neighbors to a vertex.

  Given a list (h : t) of adjacencies (vertices that are adjacenct to the
  origin vertex) we can find the list of vertices adjacent to any particular
  vertex, such as v.

  we return only the list of vertices GREATER than v.  To check
  if v0 - v1 are neighbors you need to check if the smaller's neighbor
  list contains the larger
 
  This is the underlying concept behind Vector Space Graphs.
-}
nbrs :: (Scalar a) => Integer -> [Vector a] -> Vector a -> [Vector a]
nbrs n l v = filter (> v) allnbr
             where allnbr = map (zipWith (x n) v) l

isnbr :: (Ord a, Scalar (Vector a)) => Integer -> Set.Set (Vector a) -> Vector a -> Vector a -> Bool
isnbr n l v1 v2 = Set.member (x n v1 v2) l

toIntBE :: (Num a) => a -> [a] -> a
toIntBE base num = foldl' (\acc h -> base*acc + h) (fromInteger 0) num

fromIntBE base num = fromIntBE' [] base num
                     where  fromIntBE' acc b n = let (d,m) = divMod n b
                                                      in if n == 0
                                                         then acc 
                                                         else fromIntBE' (m:acc) base d

setToArray :: (Ix a, Num a) => a -> Set.Set ([a]) -> Array a Bool
setToArray n s = array (min, max) [(i, Set.member i numset) | i <- range (min,max)]
                 where max = Set.findMax numset 
                       min = Set.findMin numset
                       numset = Set.map (toIntBE n) s

isnbr_a :: (Ix a, Integral a) => a -> Array a Bool -> [a] -> [a] -> Bool
isnbr_a base atab v1 v2 = if (vdiff <= up) && (low <= vdiff)
                     then atab ! vdiff
                     else False
                   where vdiff = toIntBE base (subvs v1 v2)
                         (low, up) = bounds atab
                         subvs v1 v2 = [ (yi-xi) `mod` base | (xi,yi) <- zip v1 v2 ]
