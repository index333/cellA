module Rule where
import Data.Bits
import GHC.Word
rule :: Int -> Int -> Bool
rule = testBit 
nextC :: Int -> (Bool,Bool,Bool) -> Bool
nextC rl (a,b,c) = rule rl i
    where i = bToI [a,b,c]
bk,wt :: (GHC.Word.Word8, GHC.Word.Word8, GHC.Word.Word8)
bk = (0,0,0)
wt = (255,255,255)
b,w :: Bool
b = True
w = False
bToI :: [Bool] -> Int
bToI bs = foldl (\x (y,z) -> if z then setBit x y else clearBit x y) (0::Int)
    $ zipWith (\a b -> (a,b)) [0 ..] $ reverse bs
