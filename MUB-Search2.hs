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


makeAdj :: Integer -> [[Integer]] -> [Integer] -> [Integer] -> Bool
makeAdj base list = isnbr_a base list_a
                  where list_a = setToArray base (Set.fromList list)

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
  let d  = read d'  :: Integer
  let n  = read n'  :: Integer
  let k  = read k'  :: Integer
  let totJobs = read totJobs' :: Integer
  let job  = read job'  :: Integer


  {- List of all vectors orthogonal to zero -}
  fOrth_l <- readFile fOrth
  let orthTab = map read (lines fOrth_l) :: [[Integer]]
  {- List of all vectors unbiased to zero -}
  fUnbias_l <- readFile fUnbias
  let unbiasTab = map read (lines fUnbias_l) :: [[Integer]]
  {- List of all possible second vectors in the first basis. -}
  fVert <- readFile fVert
  let secondVecs = map read (lines fVert) :: [[Integer]]

  {-
    Job specification.
    We break jobs up by selecting a range of second
    vectors in the first basis.  The first vector in
    the first basis is always the zero vector
  -}
  let vC = genericLength secondVecs 
  let vB | job >= totJobs = error "job number not less than totJobs" 
         | otherwise = (job, vC `div` totJobs)

  let pOrth = makeAdj n orthTab 
  let pUnbias = makeAdj n unbiasTab 
  {- Put the all zero vector in, and build the bases for this job to search. -}
  let orth_bases = concatMap k_sup_cliques (specVerts [genericReplicate (d - 1) 0] secondVecs vB)
           where k_sup_cliques = cliques n d orthTab
  
  {- To have k MUBs, we will have d*k vectors, so we need at least d*(k-1)
     vectors unbiased to the first basis -}
  let candidates = filter is_candidate orth_bases
        where is_candidate basis = (genericLength (filter (\x -> all (pUnbias x) basis) unbiasTab)) >= d*(k-1)
  {-
    Output.
  -}
  let nC = genericLength orthTab
  let s | k == 0 = error ((show nC) ++ " total adjacencies, " ++ (show vC) ++ " total vertices, " ++ (show totJobs) ++
                          " total jobs.   This is job " ++ (show job) ++ " which searches vertex block " ++ (show $ fst vB) ++
                          " (min size " ++ (show $ snd vB) ++ ").")
        | otherwise = map (putStrLn . show) candidates
  sequence_ $ s

