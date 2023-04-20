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

-- distance :: Point -> Point -> Double
-- distance x y = sqrt $ sum $ zipWith (\a b -> (a - b) ** 2) x y

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

initCentroids :: Int -> [Point] -> IO [Centroid]
initCentroids k points = do
  let n = length points
  indices <- mapM (\_ -> randomRIO (0, n-1)) [1..k]
  return $ map (points !!) indices

-- clusterError :: [Cluster] -> Double
-- clusterError clusters = sum $ concatMap clusterError' clusters
--   where
--     clusterError' cluster =
--       let centroid = calculateCentroid cluster
--       in map (distance centroid) cluster

isConverged :: [Centroid] -> [Centroid] -> Double -> Bool
isConverged oldCentroids newCentroids converged = do
    let tmp = zipWith distance oldCentroids newCentroids
    let tmp2 = foldl' (\acc x -> acc + x) 0 tmp
    if tmp2 < converged then
        True
    else
        False

printIndex :: Index -> [Point] -> Int -> Int -> IO ()
printIndex [] points n count = return ()
printIndex (x:xs) points n count = do
    if x == n then do
        putStr "("
        putStr (show (points !! count !! 0))
        putStr ","
        putStr (show (points !! count !! 1))
        putStr ") "
        putStr "("
        printOneCentroids (points !! count) 0
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

kMeans :: Int -> Double -> [Centroid] -> [Point] -> IO()
kMeans k threshold centroids points = do
  centro <- initCentroids k points
  let loop index oldCentroids = do
        let newIndex = closest oldCentroids points []
            newCentroids = calculateCentroid oldCentroids points newIndex 0 []
        if isConverged oldCentroids newCentroids threshold
          then printResult oldCentroids newIndex points 0
          else loop newIndex newCentroids
  loop [] centro

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
checkArgumentsDoubleNext str | (readMaybe str :: Maybe Double) < Just(0) || (readMaybe str :: Maybe Double) > Just(1)  = exitWith (ExitFailure 84)
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
    -- centro <- (initCentroids (read (args !! (getArg args "-n" 0)) :: Int) (parsePoints file))
    -- printCentroids centro
    res <- kMeans (read (args !! (getArg args "-n" 0)) :: Int) (read (args !! (getArg args "-l" 0)) :: Double) [] (parsePoints file)
    -- print (floor ((calculateCentroid (getCluster res 1)) !! 2))
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

-- calculateNewCentroids :: [Centroid] -> [Point] -> Index -> [Centroid]
-- calculateNewCentroids [] points index = []
-- calculateNewCentroids (x:xs) points index = do
--     let tmp = calculateCentroid (getCluster points index) 0
--     tmp : calculateNewCentroids xs points (index + 1)

calculatePoints :: Centroid -> [Point] -> Index -> Int -> Int -> Int -> Int
calculatePoints centroid points [] n res pos = res
calculatePoints centroid (y:ys) (x:xs) n res pos = do
    if x == n then
        calculatePoints centroid (y:ys) (x:xs) n (res + (centroid !! pos)) pos
    else
        calculatePoints centroid ys xs n res pos

-- calculatePoints :: Centroid -> [Point] -> Index -> Int -> Int -> Int -> Int
-- calculatePoints _ [] [] _ res _ = res
-- calculatePoints centroid (y:ys) (x:xs) n res pos
--   | x == n = calculatePoints centroid ys xs n (res + centroid !! pos) pos
--   | otherwise = calculatePoints centroid ys xs n res pos

getIndex :: Index -> Int -> Int -> Int
getIndex [] n count = count
getIndex (x:xs) n count
    | x == n = getIndex xs n (count + 1)
    | otherwise = getIndex xs n count

-- calculateCentroid :: [Centroid] -> [Point] -> Index -> Int -> [Centroid]
-- calculateCentroid [] points index n = []
-- calculateCentroid (x:xs) points index n = do
--     let nbOccur = getIndex index n
--         newOccur = (fromIntegral nbOccur)
--     let r = calculatePoints x points index n 0 2
--         r2 = (fromIntegral r / newOccur)
--     let g = calculatePoints x points index n 0 3
--         g2 = (fromIntegral g / newOccur)
--     let b = calculatePoints x points index n 0 4
--         b2 = (fromIntegral b / newOccur)
--     let tmp = [(x !! 0), (x !! 1), r2, g2, b2]
--     tmp : calculateCentroid xs points index (n + 1)

calculateCentroid :: [Centroid] -> [Point] -> Index -> Int -> [Centroid] -> [Centroid]
calculateCentroid [] points index n centro  = centro
calculateCentroid (x:xs) points index n centro = do
    let nbOccur = getIndex index n n
        newOccur = fromIntegral nbOccur :: Double
    let r = calculatePoints x points index n 0 2
        r2 = fromIntegral r / newOccur
    let g = calculatePoints x points index n 0 3
        g2 = fromIntegral g / newOccur
    let b = calculatePoints x points index n 0 4
        b2 = fromIntegral b / newOccur
    let tmp = [(x !! 0), (x !! 1), round r2, round g2, round b2]
    centro
    -- calculateCentroid xs points index (n + 1) (centro ++ [tmp])

-- calculateCentroid :: [Centroid] -> [Point] -> Index -> Int -> [Centroid] -> [Centroid]
-- calculateCentroid [] points index n centro = centro
-- calculateCentroid (x:xs) points index n centro = do
--         r <- calculatePoints x points index n 0 2
--         g <- calculatePoints x points index n 0 3
--         b <- calculatePoints x points index n 0 4
--     let nbOccur = getIndex index n 0
--         r2 = r `div` nbOccur
--         g2 = g `div` nbOccur
--         b2 = b `div` nbOccur
--         tmp = [x !! 0, x !! 1, r2, g2, b2]
--     in calculateCentroid xs points index (n + 1) (centro ++ [tmp])
