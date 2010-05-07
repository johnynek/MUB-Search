{-
  Find bases or sets of MUBs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

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


{-
  MUB-Search <d> <n> <fAdj> <fVert> <r> <k> <totaljobs> <j>

  Dimension d vector space.
  Field of scalars has size n.
  Vectors adjacent to the 0-vector read from fAdj.
  Vertices read from fVert or standard input if fVert is "-".
  Vertices are vectors if r = 2 or bases if r = 3.
  Search for k-cliques.

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
  d' : n' : fAdj : fVert : r' : k' : totJobs' : job' : _ <- getArgs
  let d  = read d'  :: Integer
  let n  = read n'  :: Integer
  let r  = read r'  :: Integer
  let k  = read k'  :: Integer
  let totJobs = read totJobs' :: Integer
  let job  = read job'  :: Integer


  {-
    Common.
  -}
  ns'' <- readFile fAdj
  vs'' <- if   fVert == "-"
          then getContents
          else readFile fVert
  let ns' = lines ns''
  let vs' = lines vs''


  {-
    Job specification.
  -}
  let nC = genericLength ns'
  let vC = genericLength vs'
  let vS = vC `div` totJobs
  let vB | job >= totJobs = error "job number not less than totJobs" 
         | otherwise = (job, vS)
  {-
    r = 2.
  -}
  let q2 = concatMap k_sup_cliques (specVerts [genericReplicate (d - 1) 0] vs vB)
           where ns = map read ns' :: [[Integer]]
                 vs = map read vs' :: [[Integer]]
                 k_sup_cliques = cliques n k ns


  {-
    r = 3.
    Below is broken, hacked to compile
  let q3 = concatMap sup_clique (specVerts [] vs vB)
           where ns = transpose $ map ((genericTake d) . repeat . read) ns' :: [[[Integer]]]
                 vs = map read vs' :: [[[Integer]]]
                 sup_clique = cliques n (k-1) ns 
  -}
  let q3 = [[[]]] :: [[[Integer]]]


  {-
    Output.
  -}
  let s | k == 0 = error ((show nC) ++ " total adjacencies, " ++ (show vC) ++ " total vertices, " ++ (show totJobs) ++
                          " total jobs.   This is job " ++ (show job) ++ " which searches vertex block " ++ (show $ fst vB) ++
                          " (min size " ++ (show $ snd vB) ++ ").")
        | k == 1 = error ((show k) ++ "-cliques are boring")
        | r == 2 = map (putStrLn . show) q2
        | r == 3 = map (putStrLn . show) q3
        | otherwise = error "Invalid rank"
  sequence_ $ s

