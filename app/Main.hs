module Main (main) where

import Text.Read (readMaybe)
import System.IO
import Data.Maybe (catMaybes)
import Data.List (transpose, minimumBy, groupBy)
import System.Exit
import System.Random (randomRIO, newStdGen, RandomGen, randomR)
import Data.List (foldl')
import System.Environment

type Point = [Double]
type Centroid = Point
type Cluster = [Point]

defaultPoint :: Point
defaultPoint = [165, 42, 42]

defaultPointOther :: Point
defaultPointOther = [180, 44, 32]

defaultListPoint :: Cluster
defaultListPoint = [defaultPoint, defaultPointOther, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint, defaultPoint]

-- distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
-- distance (x1, y1, z1) (x2, y2, z2) =
--     sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)

distance2 :: Point -> Point -> Double
distance2 x y = sqrt $ sum $ zipWith (\a b -> (a - b) ** 2) x y

myOpen :: String -> IO String
myOpen filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    return contents

parsePoint :: String -> Point
parsePoint s = map read $ words $ map (\c -> if c == ',' then ' ' else c) $ filter (/= '(') $ filter (/= ')') s

parsePoints :: String -> [Point]
parsePoints s = map parsePoint $ lines s

myHead :: String -> [Point]
myHead file = do
    let tmp = lines file
    -- let tmp6 = myParser (tmp !! 1) 2 2 []
    return []
--     print (tmp6)

--     -- print (stringToTuple (tmp !! 0))
--     -- let tmp2 = parseListToTuple tmp
--     -- print tmp2

initCentroids :: Int -> [Point] -> IO [Centroid]
initCentroids k points = do
  let n = length points
  indices <- mapM (\_ -> randomRIO (0, n-1)) [1..k]
  return $ map (points !!) indices

clusterError :: [Cluster] -> Double
clusterError clusters = sum $ concatMap clusterError' clusters
  where
    clusterError' cluster =
      let centroid = calculateCentroid cluster
      in map (distance2 centroid) cluster

isConverged :: [Centroid] -> [Centroid] -> Double -> Bool
isConverged oldCentroids newCentroids converged = do
    let tmp = zipWith distance2 oldCentroids newCentroids
    let tmp2 = foldl' (\acc x -> acc + x) 0 tmp
    if tmp2 < converged then
        True
    else
        False

kMeans :: Int -> Double -> [Point] -> IO [Cluster]
kMeans k threshold points = do
  centroids <- initCentroids k points
  let loop clusters oldCentroids = do
        let newClusters = closest centroids points
            newCentroids = map calculateCentroid newClusters
        if isConverged oldCentroids newCentroids threshold
          then putStrLn "Converged!" >> return clusters
          else loop newClusters newCentroids
  loop [] centroids

calculateSSE :: [Cluster] -> [Centroid] -> Double
calculateSSE clusters centroids =
  foldl' (\acc (c,p) -> acc + distance2 c p) 0 (zip centroids points)
  where
    points = concat clusters

checkFlags :: [String] -> Int -> Bool
checkFlags [] num = True
checkFlags (x:xs) num | (x /= "-n" && x /= "-l" && x /= "-f" && (num `mod` 2) == 0) = False
                      | otherwise = checkFlags xs (num + 1)

checkArgumentsInt :: String -> IO ()
checkArgumentsInt str | (readMaybe str :: Maybe Int) == Nothing = exitWith (ExitFailure 84)
                         | otherwise = return ()

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

main :: IO (Int)
main = do
    args <- getArgs
    parseArgs args
    checkArgumentsDouble (args !! (getArg args "-l" 0))
    checkArgumentsInt (args !! (getArg args "-n" 0))
    file <- myOpen (args !! (getArg args "-f" 0))
    -- print(parsePoints file)
    res <- kMeans (read (args !! (getArg args "-n" 0)) :: Int) (read (args !! (getArg args "-l" 0)) :: Double) (parsePoints file)
    putStrLn "--"
    putStrLn "-"
    putStrLn "--"
    putStrLn "-"
    return 0

closest :: [Centroid] -> [Point] -> [Cluster]
closest centroids points =
  let assign p = minimumBy (comparedistance2 p) centroids
      comparedistance2 p x y = compare (distance2 p x) (distance2 p y)
      distance2 x y = sqrt $ sum $ zipWith (\a b -> (a - b) ** 2) x y
  in groupBy (\a b -> assign a == assign b) points

calculateCentroid :: Cluster -> Centroid
calculateCentroid points =
  let pointCount = fromIntegral $ length points
      sums = map sum $ transpose points
  in map (/ pointCount) sums
