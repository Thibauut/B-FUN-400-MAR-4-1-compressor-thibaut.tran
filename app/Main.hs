module Main (main) where

import Text.Read (readMaybe)
import System.IO
import Data.List (transpose, minimumBy, groupBy)
import System.Random
import System.Exit
import System.Environment

type Point = [Double]
type Centroid = Point
type Cluster = [Point]

defaultPoint :: Point
defaultPoint = [165.0, 42.0, 42.0]

defaultPointOther :: Point
defaultPointOther = [180.0, 44.0, 32.0]

defaultListPoint :: Cluster
defaultListPoint = [defaultPoint, defaultPointOther, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint]

distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)

myOpen :: String -> IO String
myOpen filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    return contents

-- myParser :: [Char] -> Int -> Int -> [Point] -> [Point]
-- myParser (x:xs) a n str | x == '(' = myParser xs a n str
--                         | a == 0 = str
--                         | x == ')' = str
--                         | x == ',' = myParser xs (a - 1) n str
--                         | n == a = myParser xs a n (str ++ [x])
--                         | otherwise = myParser xs a n str

checkFlags :: [String] -> Int -> Bool
checkFlags [] num = True
checkFlags (x:xs) num | (x /= "-n" && x /= "-l" && x /= "-f" && (num `mod` 2) == 0) = False
                      | otherwise = checkFlags xs (num + 1)

parseArgs :: [String] -> IO ()
parseArgs args = do
    if length args /= 6 then
        exitWith (ExitFailure 84)
    else if checkFlags args 0 == False then
        exitWith (ExitFailure 84)
    else
        return ()

getArg :: [String] -> String -> Int -> Int
getArg [] arg num = 0
getArg (x:xs) arg num | x == arg = (num + 1)
                      | otherwise = getArg xs arg (num + 1)

main :: IO (Int)
main = do
    args <- getArgs
    parseArgs args
    file <- myOpen (args !! (getArg args "-f" 0))
    -- print res
    return 0
