import Rule
import Prelude as P
import System.Environment
import Data.Array.Repa.IO.BMP
import Data.Array.Repa as R
import Control.Parallel.Strategies
getN :: IO Int
getN = do
    a:_ <- getArgs
    return $ read a
cells = 1001
fs :: [Bool]
fs = cs P.++ [b] P.++ cs 
    where   cs = replicate n w 
            n = cells `div` 2
is = P.map (\x -> (x, x+1, x+2)) [0..(cells - 1)]
loop t a n = do
    rl <- getN
    if n == 0
    then return a
    else do
        let tt = (last t:t) P.++ [head t]
        let ta = R.fromListUnboxed (Z :. cells+2) tt:: R.Array R.U R.DIM1 Bool
        let toA = P.map (\x -> nextCell rl x ta) is 
                    `using` parListChunk 100 rdeepseq
        loop toA  (toA P.++ a ) (n-1) 
    where nextCell rl (a,b,c) tt = nextC rl (tt ! (Z :. a), tt ! (Z :. b), tt ! (Z :. c))
main = do
    bs <- loop fs fs (cells `div` 2)
    let bbs = R.fromListUnboxed (Z :. (cells*(cells `div` 2 + 1))) bs :: R.Array R.U R.DIM1 Bool 
    let a = R.map (\x -> if x then bk else wt) bbs
    computeP ( reshape (Z :. (cells `div` 2 + 1) :. cells) a) >>= (writeImageToBMP "a.bmp")

