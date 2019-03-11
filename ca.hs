import System.Environment
import Rule
fs = replicate 31 '0' ++ "010" ++ replicate 31 '0'
r s rule = let i = bToI (toBs s) in rule !! i
    where toBs = map (\x -> if x == '0' then False else True)
loop l ll rule = do
    if length l == 2
        then ll
        else loop (tail l) (ll ++ [r (take 3 l) rule]) rule
loop0 l n rule = do
    putStrLn l
    if n == 0   then return l
                else loop0 (last ll:ll++[head ll]) (n-1) rule
    where ll = loop l [] rule
getN :: IO Int
getN = getArgs >>= (\(n:[]) -> return (read n))
main = do 
    n <- getN
    rs <- readFile "rules"
    let rule = read rs !! n :: String
    loop0 fs 31 $ reverse rule
