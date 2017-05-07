import Data.List
import Control.Monad

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:xr | x <- xs, xr <- perms (xs\\[x]) ]

powerSet :: (Eq a) => Int -> [a] -> [[a]]
powerSet 0 _ = [[]]
powerSet n xs = [ x:xr | x <- xs, xr <- powerSet (n-1) xs ]

checkSums :: [Int] -> Bool
checkSums xs = 
    let x0 = xs !! 0
        x1 = xs !! 1
        x2 = xs !! 2
        x3 = xs !! 3
        x4 = xs !! 4
        x5 = xs !! 5
        x6 = xs !! 6
        v = x0 + x1
    in    v == x1+x2+x3 
       && v == x3+x4+x5 
       && v == x5+x6

fourRings :: Int -> Int -> Bool -> Bool -> IO ()
fourRings low high allowRepeats verbose = do
    let candidates = if allowRepeats
                     then powerSet 7 [low..high]
                     else perms [low..high]

        solutions = filter checkSums candidates

    when verbose $ mapM_ print solutions

    putStrLn $    show (length solutions)  
               ++ (if allowRepeats then " non" else "")
               ++ " unique solutions for " 
               ++ show low 
               ++ " to " 
               ++ show high

    putStrLn ""

main = do
   fourRings 1 7 False True
   fourRings 3 9 False True
   fourRings 0 9 True False
