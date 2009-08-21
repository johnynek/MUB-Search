{-
  Butson graphs: construction and clique-finding.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Graph (Vert, Clique, Graph, Vec, AdjF, graph, graphU, cliques, rootcliques) where

import Magic
import Data.Set


{-
  Type definitions.
-}
type Vert   = Int
type Clique = Set Vert
type Graph  = [Set Vert]
type Vec    = [Int]
type AdjF   = Set Int


{-
  Compute the pointwise difference (mod n) between two vectors x and y.
-}
pointDiff :: Int -> Vec -> Vec -> Vec
pointDiff n a b = zipWith (\x y -> mod (x - y) n) a b


{-
  Given adjacency function adjF, find the set of all vertices adjacent to a vertex v.
-}
neighbors :: (Int, Int) -> AdjF -> Vert -> Set Vert
neighbors (d, n) f v = Data.Set.map (\u -> vec2magic (d, n) (pointDiff n x (magic2vec (d, n) u))) f
                       where x = magic2vec (d, n) v


{-
  A list of sets describing adjacency of vertices.

  Index k in the list is a set containing all vertices adjacent to vertex k
  that are numbered greater than k.
-}
graph :: (Int, Int) -> AdjF -> [Set Vert]
graph (d, n) f = [ Data.Set.filter (> v) (neighbors (d, n) f v) | v <- [0 .. (n^d) - 1] ]

graphU :: (Int, Int) -> AdjF -> [Set Vert]
graphU (d, n) f = [ neighbors (d, n) f v | v <- [0 .. (n^d) - 1] ]


{-
  Compute the intersection of all sets in a set.
-}
intersections s = if   1 == (Data.Set.size s)
                  then m
                  else intersection m (intersections t)
                  where (m, t) = deleteFindMin s


{-
  Given a graph g and clique c, find the set of all vertices that extend c.

  A vertex that extends a clique is a vertex that is adjacent to all
  vertices in that clique.
-}
extendingVerts :: Graph -> Clique -> Set Vert
extendingVerts g c = intersections (Data.Set.map (g !!) c)


{-
  Given a graph g and a list of cliques s, find all cliques in g that
  contain a clique from s and one additional vertex.
-}
biggerCliques :: Graph -> [Clique] -> [Clique]
biggerCliques g s = [insert w y | y <- s, w <- elems (extendingVerts g y)]


{-
  Given a graph g, find all cliques of size n.
-}
cliques :: Graph -> Int -> [Clique]
cliques g 0  = [empty]
cliques g n  = (iterate (biggerCliques g) [empty]) !! n


{-
  Given a graph g, find all cliques of size n that
  include vertex 0.
-}
rootcliques :: Graph -> Int -> [Clique]
rootcliques g 0 = [empty]
rootcliques g n = (iterate (biggerCliques g) [singleton 0]) !! (n - 1)

