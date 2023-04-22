module Main (main) where

import Text.Read (readMaybe)
import System.IO
import Data.Maybe (catMaybes)
import Data.List (transpose, minimumBy, groupBy)
import System.Exit
import System.Random (randomRIO, newStdGen, RandomGen, randomR)
import Data.List (foldl')
import System.Environment

type Point = [Int]
type Centroid = Point
type Cluster = [Point]
type Index = [Int]

distance :: [Int] -> [Int] -> Double
distance p1 p2 =
    sqrt (((fromIntegral (p1 !! 2)) - (fromIntegral (p2 !! 2))) ** 2 + ((fromIntegral (p1 !! 3)) - (fromIntegral (p2 !! 3))) ** 2 + ((fromIntegral (p1 !! 4)) - (fromIntegral (p2 !! 4))) ** 2)

myOpen :: String -> IO String
myOpen filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    return contents

parsePoint :: String -> Point
parsePoint s = map read $ words $ map (\c -> if c == ',' then ' ' else c) $ filter (/= '(') $ filter (/= ')') s

parsePoints :: String -> [Point]
parsePoints s = map parsePoint $ lines s

printCentroids :: [Centroid] -> IO ()
printCentroids [] = return ()
printCentroids (x:xs) = do
  putStr "--\n("
  printOneCentroids x 0
  putStrLn ")\n-"
  printCentroids xs

printOneCentroids :: Centroid -> Int -> IO ()
printOneCentroids [] n = return ()
printOneCentroids (x:xs) n = do
  if n == 2 || n == 3 then do
    putStr (show x)
    putStr ","
    printOneCentroids xs (n + 1)
  else
    if n == 4 then do
      putStr (show x)
    else
     printOneCentroids xs (n + 1)

getColorCentroid :: Centroid -> [Int]
getColorCentroid centroid = [centroid !! 2, centroid !! 3, centroid !! 4]

checkSameCentroid :: [Centroid] -> Centroid -> Bool
checkSameCentroid [] centroid = False
checkSameCentroid (x:xs) centroid | (getColorCentroid x) == (getColorCentroid centroid) = True
                                  | otherwise = checkSameCentroid xs centroid

initCentroids :: Int -> Int -> Int -> [Point] -> [Centroid] -> IO [Centroid]
initCentroids k count pos points centro = do
    if count == k then
        return centro
    else do
        rand <- randomRIO (0, (length points - 1))
        if (checkSameCentroid centro (getPoint points rand 0)) == True then
            initCentroids k count (pos + 1) points centro
        else do
            let centrop = centro ++ [(getPoint points rand 0)]
            initCentroids k (count + 1) (pos + 1) points centrop


isConverged :: [Centroid] -> [Centroid] -> Double -> Bool
isConverged oldCentroids newCentroids converged = do
    let tmp = zipWith distance oldCentroids newCentroids
    let tmp2 = foldl' (\acc x -> acc + x) 0 tmp
    if tmp2 > converged then
        True
    else
        False

getPoint :: [Point] -> Int -> Int -> Point
getPoint (x:xs) pos count | count == pos = x
                          | otherwise = getPoint xs pos (count + 1)

printIndex :: Index -> [Point] -> Int -> Int -> IO ()
printIndex [] points n count = return ()
printIndex (x:xs) points n count = do
    if x == n then do
        putStr "("
        putStr (show ((getPoint points count 0) !! 0))
        putStr ","
        putStr (show ((getPoint points count 0) !! 1))
        putStr ") "
        putStr "("
        printOneCentroids (getPoint points count 0) 0
        putStrLn ")"
        printIndex xs points n (count + 1)
    else
        printIndex xs points n (count + 1)

printResult :: [Centroid] -> Index -> [Point] -> Int -> IO ()
printResult [] index points n = return ()
printResult (x:xs) index points n = do
    putStr "--\n("
    printOneCentroids x 0
    putStrLn ")\n-"
    printIndex index points n 0
    printResult xs index points (n + 1)

-- initMyCentroids :: [Centriud] -> [Points] -> [Centroid]
-- initMyCentroids centro = do

kMeans :: Int -> Double -> [Centroid] -> [Point] -> IO()
kMeans k threshold centroids points = do
  centro <- initCentroids k 0 0 points []
  let loop index oldCentroids = do
        let newIndex = closest oldCentroids points []
            newCentroids = calculateCentroid oldCentroids points newIndex 0 []
        -- print "hey 1"
        -- print oldCentroids
        -- -- print newIndex
        -- -- print newIndex
        -- -- print points
        -- print "hey 2"
        if isConverged oldCentroids newCentroids threshold == True
          then printResult oldCentroids newIndex points 0
          else loop newIndex newCentroids
  loop [] centro
  return ()

checkFlags :: [String] -> Int -> Bool
checkFlags [] num = True
checkFlags (x:xs) num | (x /= "-n" && x /= "-l" && x /= "-f" && (num `mod` 2) == 0) = False
                      | otherwise = checkFlags xs (num + 1)

checkArgumentsIntNext :: String -> IO ()
checkArgumentsIntNext str | (readMaybe str :: Maybe Int) < Just(1) || (readMaybe str :: Maybe Int) > Just(255)  = exitWith (ExitFailure 84)
                          | otherwise = return ()

checkArgumentsInt :: String -> IO ()
checkArgumentsInt str | (readMaybe str :: Maybe Int) == Nothing = exitWith (ExitFailure 84)
                      | otherwise = checkArgumentsIntNext str

checkArgumentsDoubleNext :: String -> IO()
checkArgumentsDoubleNext str | (readMaybe str :: Maybe Double) <= Just(0) || (readMaybe str :: Maybe Double) > Just(1)  = exitWith (ExitFailure 84)
                             | otherwise = return ()

checkArgumentsDouble :: String -> IO ()
checkArgumentsDouble str | (readMaybe str :: Maybe Double) == Nothing = exitWith (ExitFailure 84)
                         | otherwise = checkArgumentsDoubleNext str

checkNumberOfColors :: [Cluster] -> Int -> Bool
checkNumberOfColors [] num = True
checkNumberOfColors (x:xs) num | (length x) > num = False
                               | otherwise = checkNumberOfColors xs num

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

getCluster :: [Cluster] -> Int -> Cluster
getCluster [] num = []
getCluster (x:xs) num | num == 0 = x
                      | otherwise = getCluster xs (num - 1)

main :: IO (Int)
main = do
    args <- getArgs
    parseArgs args
    checkArgumentsDouble (args !! (getArg args "-l" 0))
    checkArgumentsInt (args !! (getArg args "-n" 0))
    file <- myOpen (args !! (getArg args "-f" 0))
    res <- kMeans (read (args !! (getArg args "-n" 0)) :: Int) (read (args !! (getArg args "-l" 0)) :: Double) [] (parsePoints file)
    return 0

findClosest :: [Centroid] -> Point -> Double -> Int -> Int -> Int
findClosest [] point n index count = index
findClosest (x:xs) point n index count = do
    let tmp = distance x point
    if tmp < n then
        findClosest xs point tmp count (count + 1)
    else
        findClosest xs point n index (count + 1)

closest :: [Centroid] -> [Point] -> Index -> Index
closest centroids [] index = index
closest centroids (x:xs) index = do
    let tmp = findClosest centroids x 10000.0 0 0
    tmp : closest centroids xs index

calculateR :: Int -> [Point] -> Index -> Int -> Int
calculateR pos [] [] res = res
calculateR pos (p:ps) (i:is) res | i == pos = calculateR pos ps is (res + (p !! 2))
                                 | otherwise = calculateR pos ps is res

calculateG :: Int -> [Point] -> Index -> Int -> Int
calculateG pos [] [] res = res
calculateG pos (p:ps) (i:is) res | i == pos = calculateG pos ps is (res + (p !! 3))
                                 | otherwise = calculateG pos ps is res

calculateB :: Int -> [Point] -> Index -> Int -> Int
calculateB pos [] [] res = res
calculateB pos (p:ps) (i:is) res | i == pos = calculateB pos ps is (res + (p !! 4))
                                 | otherwise = calculateB pos ps is res

getIndex :: Index -> Int -> Int -> Int
getIndex [] n count = count
getIndex (x:xs) n count
    | x == n = getIndex xs n (count + 1)
    | otherwise = getIndex xs n count

getOcc :: Index -> Int -> Int
getOcc [] n = 0
getOcc (x:xs) n | x == n = 1 + getOcc xs n
                | otherwise = getOcc xs n

calculateCentroid :: [Centroid] -> [Point] -> Index -> Int -> [Centroid] -> [Centroid]
calculateCentroid [] points index n centro = centro
calculateCentroid (x:xs) points index n centro = do
    let occ = getOcc index n
    let r = calculateR n points index 0
    let g = calculateG n points index 0
    let b = calculateB n points index 0
    let tmp = [0, 0, r `div` occ , g `div` occ, b `div` occ]
    calculateCentroid xs points index (n + 1) (tmp : centro)
