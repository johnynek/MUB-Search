import Data.List

uniqPerms :: (Eq a) => [a] -> [[a]]
uniqPerms = nub . permutations

main = do
         content <- getContents
         let vecs = map read (lines content) :: [[Int]]
         let all = concatMap uniqPerms vecs
         sequence_ $ map (putStrLn . show) all 
