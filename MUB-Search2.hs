{-
  Find bases or sets of MUBs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List
import qualified Data.Set as Set
import Cliques


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

-- This requires base^d entries, but is very fast
makeAdj :: Int -> [[Int]] -> [Int] -> [Int] -> Bool
makeAdj base list = isnbr_a base list_a
                  where list_a = setToArray base (Set.fromList list)
-- This is space optimal, but O(log N) time to look up.
makeAdjSet :: Int -> [[Int]] -> [Int] -> [Int] -> Bool
makeAdjSet base list x y = Set.member diffv s
                         where diffv = toI [ (yi-xi) `mod` base | (xi,yi) <- zip x y]
                               s = Set.fromList (map toI list)
                               toI larg = toIntBE base (sort larg)

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast x l = (length (take x l)) == x

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
basisExt uAdj oAdj ul (ubs, h:t, clique) = if ext
                                           then (h_ub, h_orth, h:clique):(basisExt uAdj oAdj ul (ubs, t, clique))
                                           else basisExt uAdj oAdj ul (ubs, t, clique)
                                  where h_ub = filter (uAdj h) ubs -- all in ubs unbiased to h
                                        h_orth = filter (oAdj h) t -- all in t orth to h
                                        ext = lengthAtLeast ul h_ub -- there are sufficient unbiased vects

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
            singles' acc (x:xs) = (x, acc ++ xs):(singles' (acc ++ [x]) xs)

-- Take a basis candidate (orthogonality clique is size d)
makeUB :: IntListAdj -> IntListAdj -> Candidate -> [Candidate]
makeUB _ _ ([],_,_) = []
makeUB uAdj oAdj (ucand,ocand,clique) = concatMap next init
                        where init = [(makeCand uAdj oAdj (snd sings) (snd sings) [(fst sings)]) | sings <- (singles ucand)]
                              next = makeBasis uAdj oAdj 0
{-
  Take a Candidate, extend them into full bases, and then 
  [a] -> (a -> [a]) -> [[a]]

  iterate2 :: (a -> [a]) -> a -> [a]
  [([x],[x0,x1,...]),([x0,x],[f(x0)]),
-}
make4MUBs :: IntListAdj -> IntListAdj -> Candidate -> [(Basis,Basis,Basis,Basis)]
make4MUBs uAdj oAdj cand = do
                     child0 <- makeBasis uAdj oAdj 1 cand;
                     child1 <- makeUB uAdj oAdj child0;
                     child2 <- makeUB uAdj oAdj child1;
                     child3 <- makeUB uAdj oAdj child2;
                     return ((getBasis child0), (getBasis child1), (getBasis child2), (getBasis child3))

make3MUBs :: IntListAdj -> IntListAdj -> Candidate -> [(Basis,Basis,Basis)]
make3MUBs uAdj oAdj cand = do
                     child0 <- makeBasis uAdj oAdj 1 cand;
                     child1 <- makeUB uAdj oAdj child0;
                     child2 <- makeUB uAdj oAdj child1;
                     return ((getBasis child0), (getBasis child1), (getBasis child2))

make2MUBs :: IntListAdj -> IntListAdj -> Candidate -> [(Basis,Basis)]
make2MUBs uAdj oAdj cand = do
                     child0 <- makeBasis uAdj oAdj 1 cand;
                     child1 <- makeUB uAdj oAdj child0;
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
         | otherwise = (job, vC `div` totJobs)

  let pOrth = makeAdjSet n orthTab 
  let pUnbias = makeAdjSet n unbiasTab 
  {- Put the all zero vector in, and build the bases for this job to search. -}
  let init_verts = specVerts [replicate (d - 1) 0] secondVecs vB
  let init_cand = map (makeCand pUnbias pOrth unbiasTab orthTab) init_verts
  let orth_bases = concatMap (make3MUBs pUnbias pOrth) init_cand
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

