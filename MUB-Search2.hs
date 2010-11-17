{-
  Find bases or sets of MUBs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List
import qualified Data.Set as Set
import Cliques
-- import Debug.Trace
import Array


{-
  Determine how much work this particular process will do, and form the
  starts of the cliques that it will extend.
-}
specAdjs :: [a] -> (Int, Int) -> [a]
specAdjs l (b, s) = map (genericIndex l) [i .. t]
                    where i = b * s
                          t = if   i + (s * 2) >= (length l)
                              then (length l) - 1
                              else i + s - 1

specVerts :: [a] -> [a] -> (Integer, Integer) -> [[a]]
specVerts z l (b, s) = map (\x -> (genericIndex l x) : z) [i .. t]
                       where i = b * s
                             len = genericLength l
                             {- If the next block would go past the end
                                of the list, this is the last -}
                             t = if i + (s * 2) >= len
                                 then len - 1
                                 else i + s - 1
-- Fold two lists together:
foldl2' :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
foldl2' _    zero [] _   = zero `seq` zero
foldl2' _    zero _ []   = zero `seq` zero
foldl2' step zero (x:xs) (y:ys)  =
    let new = step zero x y
    in  new `seq` foldl2' step new xs ys
-- Convert a root-of-unity dot-product to an integer:
-- <a, b> -> Int
ruFold :: Int -> Int -> Int -> Int -> Int
ruFold base acc h k = base*acc + ((k - h) `mod` base)
-- ruFold base acc h k = let dif = k - h
--                          md = dif `seq` (dif `mod` base)
--                          bp = md `seq` base * acc
--                          sm = bp `seq` (bp + md)
--                      in sm
toRUDot :: Int -> [Int] -> [Int] -> Int
toRUDot base = foldl2' (ruFold base) 0 
-- toRUDot base a_vec b_vec = foldl2' (ruFold base) 0 a_vec b_vec

-- This requires base^d entries, but is very fast
makeAdjTab :: Int -> [[Int]] -> Array Int Bool
makeAdjTab base list = setToArray base (Set.fromList list)
makeAdj :: Int -> Array Int Bool -> [Int] -> [Int] -> Bool
makeAdj = isnbr_a 
-- This is space optimal, but O(log N) time to look up.
makeAdjSetTab :: Int -> [[Int]] -> Set.Set Int
makeAdjSetTab base list = Set.fromList (map toI list)
                        --where toI larg = toIntBE base (sort larg)
                        where toI = toIntBE base

makeAdjSet :: ([Int] -> [Int] -> Int) -> Set.Set Int -> [Int] -> [Int] -> Bool
--makeAdjSet base s x y = Set.member diffv s
makeAdjSet ubf s x y = Set.member (ubf x y) s
                         -- where diffv = toI [ (yi-xi) `mod` base | (xi,yi) <- zip x y]
                         --       toI larg = toIntBE base larg
                         --where diffv = toRUDot base x y

lengthAtLeast :: Int -> [a] -> Bool
-- lengthAtLeast x l = (length (take x l)) == x
lengthAtLeast x [] | x > 0 = False
lengthAtLeast 0 _ = True
lengthAtLeast x (y:ys) = lengthAtLeast (x-1) ys
-- TODO you might want this:
-- lengthAtLeast x l = True

type IntListAdj = [Int] -> [Int] -> Bool
type Basis = [[Int]]
{-
first is the list of vectors potentially unbiased to the the candidate vectors
second is the list of vectors potentially orthogonal to the candidate vectors
third is the list of candidate vectors
-}
type Candidate  = ([[Int]], [[Int]], Basis)
getBasis :: Candidate -> Basis
getBasis (_,_,basis) = basis
{-
Take the unbiased IntListAdj
         orth IntListAdj
         minimum length of unbiased result in candidate
         a single Candidate
    return all extensions of this candidate
-}
basisExt :: IntListAdj -> IntListAdj -> Int -> Candidate -> [Candidate]
basisExt _ _ _ (_, [], _) = []
basisExt uAdj oAdj ul (ubs, h:t, clique) = let h_ub = filter (uAdj h) ubs -- all in ubs unbiased to h
                                               h_orth = filter (oAdj h) t -- all in t orth to h
                                               ext = lengthAtLeast ul h_ub -- there are sufficient unbiased vects
                                           in if ext
                                              then (h_ub, h_orth, h:clique):(basisExt uAdj oAdj ul (ubs, t, clique))
                                              else basisExt uAdj oAdj ul (ubs, t, clique)

makeCand :: IntListAdj -> IntListAdj -> [[Int]] -> [[Int]] -> [[Int]] -> Candidate
-- makeCand _ _ _ _ v | not (any (all (==0)) v) = error "Must contain zero vector"
makeCand uAdj oAdj ubZ oZ vects = (ubcand, ocand, vects) 
                          where ubcand = filter (trueForAll uAdj vects) ubZ
                                ocand = filter (trueForAll oAdj vects) oZ
                                trueForAll adj l v = all (adj v) l
{-
 Need a function to take a Candidate and return a list of bases (complete candidates).
 Need another function that takes a candidate and returns all bases unbiased to the candidate
 Need a function that takes an initial candidate, an integer and returns a list of lists of unbiased
   candidates exactly the length of the integer.
 Run the above for the given number, and print them out.
-}

-- Extends the candidates to full basis candidates. Initial cliques must have the same size
makeBasis :: IntListAdj -> IntListAdj -> Int -> Candidate -> [Candidate]
makeBasis uAdj oAdj ul init_cand = all_cand
               where (ubc,oc,clique) = init_cand 
                     current_size = length clique
                     dim = length (clique !! 0) + 1 --dimension is length + 1 of each clique member:
                     iterate_depth = dim - current_size
                     child_cand = basisExt uAdj oAdj ul
                     cand_ex = concatMap child_cand
                     all_cand = (iterate cand_ex [init_cand]) !! iterate_depth
-- Return a list of single items from a list as a set, with the compliment:
singles :: [a] -> [(a,[a])]
singles [] = []
singles ys = singles' [] ys
      where singles' acc [y] = [(y,acc)]
            singles' acc (x:xs) = (x, acc ++ xs):(singles' (x:acc) xs)
--rest [1,2,3] = [(1,[2,3]), (2,[3]), (3,[])]
rest :: [a] -> [(a, [a])]
rest [] = []
rest (y:ys) = (y, ys):(rest ys)

-- Take a basis candidate (orthogonality clique is size d)
makeUB :: IntListAdj -> IntListAdj -> Int -> Candidate -> [Candidate]
makeUB _ _ ul (ucand,_,_) | (length ucand) < ul = []
makeUB uAdj oAdj ul (ucand,ocand,clique) = concatMap next init
                        --where init = [(makeCand uAdj oAdj (snd sings) (snd sings) [(fst sings)]) | sings <- (singles ucand)]
                        -- construct the basis in order, so we don't needlessly repeat, using rest:
                        where init = [(makeCand uAdj oAdj ucand (snd sings) [(fst sings)]) | sings <- (rest ucand)]
                              next = makeBasis uAdj oAdj ul
{-
  Take a Candidate, extend them into full bases, and then 
  [a] -> (a -> [a]) -> [[a]]

  iterate2 :: (a -> [a]) -> a -> [a]
  [([x],[x0,x1,...]),([x0,x],[f(x0)]),
-}
make4MUBs :: IntListAdj -> IntListAdj -> Int -> Candidate -> [(Basis,Basis,Basis,Basis)]
make4MUBs uAdj oAdj d cand = do
                     child0 <- makeBasis uAdj oAdj (3*d) cand;
                     child1 <- makeUB uAdj oAdj (2*d) child0;
                     child2 <- makeUB uAdj oAdj d child1;
                     child3 <- makeUB uAdj oAdj 0 child2;
                     return ((getBasis child0), (getBasis child1), (getBasis child2), (getBasis child3))

make3MUBs :: IntListAdj -> IntListAdj -> Int -> Candidate -> [(Basis,Basis,Basis)]
make3MUBs uAdj oAdj d cand = do
                     child0 <- makeBasis uAdj oAdj (2*d) cand;
                     child1 <- makeUB uAdj oAdj d child0;
                     child2 <- makeUB uAdj oAdj 0 child1;
                     return ((getBasis child0), (getBasis child1), (getBasis child2))

make2MUBs :: IntListAdj -> IntListAdj -> Int -> Candidate -> [(Basis,Basis)]
make2MUBs uAdj oAdj d cand = do
                     child0 <- makeBasis uAdj oAdj d cand;
                     child1 <- makeUB uAdj oAdj 0 child0;
                     return ((getBasis child0), (getBasis child1))

{-
  MUB-Search2 <d> <n> <fOrth> <fUnbias> <fVert> <k> <totaljobs> <j>

  Dimension d vector space.
  Field of scalars has size n.
  Vectors adjacent to the 0-vector read from fAdj.
  Vertices read from fVert or standard input if fVert is "-".
  Vertices are vectors if r = 2 or bases if r = 3.
  Search for k-MUBS.

  The search is split into jobs.  Each job looks for cliques that at least
  contain a vertex from a specific block of vS vertices.

  If there are vC vertices in fVert then there are (div vC vS) blocks of vS
  vertices (the last block may contain more in order to cover all vertices).

  This process performs job #j, which is vertex block j.
-}
main = do
  {-
    Command line arguments.
  -}
  d' : n' : fOrth : fUnbias : fVert : k' : totJobs' : job' : _ <- getArgs
  let d  = read d'  :: Int
  let n  = read n'  :: Int
  let k  = read k'  :: Int -- number of MUBs
  let totJobs = read totJobs' :: Integer
  let job  = read job'  :: Integer


  {- List of all vectors orthogonal to zero -}
  fOrth_l <- readFile fOrth
  let orthTab = map read (lines fOrth_l) :: [[Int]]
  {- List of all vectors unbiased to zero -}
  fUnbias_l <- readFile fUnbias
  let unbiasTab = map read (lines fUnbias_l) :: [[Int]]
  {- List of all possible second vectors in the first basis. -}
  fVert <- readFile fVert
  let secondVecs = map read (lines fVert) :: [[Int]]

  {-
    Job specification.
    We break jobs up by selecting a range of second
    vectors in the first basis.  The first vector in
    the first basis is always the zero vector
  -}
  let vC = genericLength secondVecs 
  let vB | job >= totJobs = error "job number not less than totJobs" 
         | vC < totJobs = error ("Can't divide into more jobs than " ++ (show vC))
         | otherwise = (job, vC `div` totJobs)
  let orthSTab = makeAdjSetTab n orthTab
  let unbiasSTab = makeAdjSetTab n unbiasTab
  let !innerprod = toRUDot n
  let !pOrth = makeAdjSet innerprod orthSTab 
  let !pUnbias = makeAdjSet innerprod unbiasSTab 
  {- Put the all zero vector in, and build the bases for this job to search. -}
  let init_verts = specVerts [replicate (d - 1) 0] secondVecs vB
  -- let init_cand = traceShow init_verts $ map (makeCand pUnbias pOrth unbiasTab orthTab) init_verts
  let init_cand = map (makeCand pUnbias pOrth unbiasTab orthTab) init_verts
  -- let orth_bases = traceShow init_cand $ take 1000000 $ concatMap (make4MUBs pUnbias pOrth d) init_cand
  let orth_bases = concatMap (make3MUBs pUnbias pOrth d) init_cand
{-
  --let iterate_depth = d - (genericLength init_verts)
  let iterate_depth = d - 2 -- We have all zero, and one initial vectors
  let all_cand = (iterate cand_ex init_cand) `genericIndex` iterate_depth
               where cand_ex = concatMap child_cand
                     child_cand = basisExt pUnbias pOrth (d*(k-1))
  let orth_bases = [ basis | (_,_,basis) <- all_cand ]
-}
  {-
    Output.
  -}
  let nC = genericLength orthTab
  let s | k == 0 = error ((show nC) ++ " total adjacencies, " ++ (show vC) ++ " total vertices, " ++ (show totJobs) ++
                          " total jobs.   This is job " ++ (show job) ++ " which searches vertex block " ++ (show $ fst vB) ++
                          " (min size " ++ (show $ snd vB) ++ ").")
        | otherwise = map (putStrLn . show) orth_bases 
        -- | otherwise = map (putStrLn . show) all_cand
  sequence_ $ s
  putStrLn "#Done"

