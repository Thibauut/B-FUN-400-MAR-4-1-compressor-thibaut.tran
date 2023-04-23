{--
-- EPITECH PROJECT, 2022
-- Visual Studio Live Share (Workspace)
-- File description:
-- Main.hs
--}

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

euclideanDist :: Int -> Int -> Int -> Int -> Int -> Int -> Float
euclideanDist x1 x2 y1 y2 z1 z2 = let r = (fromIntegral x1 - fromIntegral x2)^2
                                      g = (fromIntegral y1 - fromIntegral y2)^2
                                      b = (fromIntegral z1 - fromIntegral z2)^2
                                  in sqrt $ r + g + b

distance :: [Int] -> [Int] -> Float
distance p1 p2 = let r = (fromIntegral (p1 !! 2)) - (fromIntegral (p2 !! 2))
                     g = (fromIntegral (p1 !! 3)) - (fromIntegral (p2 !! 3))
                     b = (fromIntegral (p1 !! 4)) - (fromIntegral (p2 !! 4))
                 in sqrt $ r ** 2 + g ** 2 + b ** 2

myOpen :: String -> IO String
myOpen filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    return contents

parsePoint :: String -> Point
parsePoint s = map read $ words
                        $ map (\c -> if c == ',' then ' ' else c)
                        $ filter (/= '(') $ filter (/= ')') s

parsePoints :: String -> [Point]
parsePoints s = map parsePoint $ lines s

printCentroids :: [Centroid] -> IO ()
printCentroids [] = return ()
printCentroids (x:xs) =
  putStr "--\n(" >>
  printOneC x 0 >>
  putStrLn ")\n-" >>
  printCentroids xs

printOneC :: Centroid -> Int -> IO ()
printOneC [] n = return ()
printOneC (x:xs) 2 = putStr (show x) >> putStr "," >> printOneC xs (2 + 1)
printOneC (x:xs) 3 = putStr (show x) >> putStr "," >> printOneC xs (3 + 1)
printOneC (x:xs) 4 = putStr (show x)
printOneC (x:xs) n = printOneC xs (n + 1)

getColorC :: Centroid -> [Int]
getColorC centroid = [centroid !! 2, centroid !! 3, centroid !! 4]

checkSameC :: [Centroid] -> Centroid -> Bool
checkSameC [] centroid = False
checkSameC (x:xs) centroid | (getColorC x) == (getColorC centroid) = True
                                  | otherwise = checkSameC xs centroid

initCentroids :: Int -> Int -> Int -> [Point] -> [Centroid] -> IO [Centroid]
initCentroids k count pos points centro
    | count == k = return centro
    | otherwise = do
        rand <- randomRIO (0, (length points - 1))
        if (checkSameC centro (getPoint points pos 0)) == True then
            initCentroids k count (pos + 1) points centro
        else let centrop = centro ++ [(getPoint points pos 0)]
             in initCentroids k (count + 1) (pos + 1) points centrop


checkTwoCentroids :: Centroid -> Centroid -> Float -> Bool
checkTwoCentroids old new convergence
  | distance old new < convergence = True
  | otherwise = False

checkConvergence :: [Centroid] -> [Centroid] -> Float -> Bool
checkConvergence [] [] convergence = True
checkConvergence (old:oCentroids) (new:nCentroids) convergence
  | checkTwoCentroids old new convergence == False = False
  | otherwise = checkConvergence oCentroids nCentroids convergence

isConverged :: [Centroid] -> [Centroid] -> Float -> Float
isConverged (o:os) (n:ns) c
  | length os == 0 = 0
  | (euclideanDist (o !! 2) (n !! 2) (o !! 3) (n !! 3) (o !! 4) (n !! 4)) > c =
     (euclideanDist (o !! 2) (n !! 2) (o !! 3) (n !! 3) (o !! 4) (n !! 4))
  | otherwise = isConverged os ns c

getPoint :: [Point] -> Int -> Int -> Point
getPoint (x:xs) pos count | count == pos = x
                          | otherwise = getPoint xs pos (count + 1)

printIndex :: Index -> [Point] -> Int -> Int -> IO ()
printIndex [] points n count = return ()
printIndex (x:xs) points n count
  | x == n =
        putStr "(" >> putStr (show ((getPoint points count 0) !! 0)) >>
        putStr "," >> putStr (show ((getPoint points count 0) !! 1)) >>
        putStr ") " >> putStr "(" >> printOneC (getPoint points count 0) 0 >>
        putStrLn ")" >> printIndex xs points n (count + 1)
  | otherwise = printIndex xs points n (count + 1)

printResult :: [Centroid] -> Index -> [Point] -> Int -> IO ()
printResult [] index points n = return ()
printResult (x:xs) index points n =
    putStr "--\n(" >> printOneC x 0 >> putStrLn ")\n-" >>
    printIndex index points n 0 >> printResult xs index points (n + 1)

kMeans :: Int -> Float -> [Centroid] -> [Point] -> IO ()
kMeans k threshold centroids points = do
  centro <- initCentroids k 0 0 points []
  let loop index oldCentroids =
        let newIndex = closest oldCentroids points []
            newCentroids = calculateCentroid oldCentroids points newIndex 0
        in if checkConvergence oldCentroids newCentroids threshold
            then printResult oldCentroids newIndex points 0
            else loop newIndex newCentroids
  loop [] centro

checkFlags :: [String] -> Int -> Bool
checkFlags [] num = True
checkFlags (x:xs) num
  | (x /= "-n" && x /= "-l" && x /= "-f" && (num `mod` 2) == 0) = False
  | otherwise = checkFlags xs (num + 1)

checkArgumentsIntNext :: String -> IO ()
checkArgumentsIntNext str
  | (readMaybe str :: Maybe Int) < Just(1) ||
    (readMaybe str :: Maybe Int) > Just(255)  = exitWith (ExitFailure 84)
  | otherwise = return ()

checkArgumentsInt :: String -> IO ()
checkArgumentsInt str
  | (readMaybe str :: Maybe Int) == Nothing = exitWith (ExitFailure 84)
  | otherwise = checkArgumentsIntNext str

checkArgumentsFloatNext :: String -> IO()
checkArgumentsFloatNext str
  | (readMaybe str :: Maybe Float) <= Just(0) = exitWith (ExitFailure 84)
  | otherwise = return ()

checkArgumentsFloat :: String -> IO ()
checkArgumentsFloat str
  | (readMaybe str :: Maybe Float) == Nothing = exitWith (ExitFailure 84)
  | otherwise = checkArgumentsFloatNext str

checkNumberOfColors :: [Cluster] -> Int -> Bool
checkNumberOfColors [] num = True
checkNumberOfColors (x:xs) num | (length x) > num = False
                               | otherwise = checkNumberOfColors xs num

parseArgs :: [String] -> IO ()
parseArgs args
  | length args /= 6 = exitWith (ExitFailure 84)
  | checkFlags args 0 == False = exitWith (ExitFailure 84)
  | otherwise = return ()

getArg :: [String] -> String -> Int -> Int
getArg [] arg num = 0
getArg (x:xs) arg num | x == arg = (num + 1)
                      | otherwise = getArg xs arg (num + 1)

getCluster :: [Cluster] -> Int -> Cluster
getCluster [] num = []
getCluster (x:xs) 0 = x
getCluster (x:xs) num = getCluster xs (num - 1)

main :: IO (Int)
main = do
    args <- getArgs
    parseArgs args
    checkArgumentsFloat (args !! (getArg args "-l" 0))
    checkArgumentsInt (args !! (getArg args "-n" 0))
    file <- myOpen (args !! (getArg args "-f" 0))
    res <- kMeans (read (args !! (getArg args "-n" 0)) :: Int)
      (read (args !! (getArg args "-l" 0)) :: Float) [] (parsePoints file)
    return 0

findClosest :: [Centroid] -> Point -> Float -> Int -> Int -> Int
findClosest [] point n index count = index
findClosest (x:xs) point n index count = let tmp = distance point x
    in if tmp < n then
        findClosest xs point tmp count (count + 1)
    else
        findClosest xs point n index (count + 1)

closest :: [Centroid] -> [Point] -> Index -> Index
closest centroids [] index = index
closest centroids (x:xs) index = let tmp = findClosest centroids x 10000.0 0 0
    in tmp : closest centroids xs index

calculateR :: Int -> [Point] -> Index -> Int -> Int
calculateR pos [] [] res = res
calculateR pos (p:ps) (i:is) res
  | i == pos = calculateR pos ps is (res + (p !! 2))
  | otherwise = calculateR pos ps is res

calculateG :: Int -> [Point] -> Index -> Int -> Int
calculateG pos [] [] res = res
calculateG pos (p:ps) (i:is) res
  | i == pos = calculateG pos ps is (res + (p !! 3))
  | otherwise = calculateG pos ps is res

calculateB :: Int -> [Point] -> Index -> Int -> Int
calculateB pos [] [] res = res
calculateB pos (p:ps) (i:is) res
  | i == pos = calculateB pos ps is (res + (p !! 4))
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

calculateCentroid :: [Centroid] -> [Point] -> Index -> Int -> [Centroid]
calculateCentroid [] points index n = []
calculateCentroid (x:xs) points index n =
    let occ = getOcc index n
        r = calculateR n points index 0
        g = calculateG n points index 0
        b = calculateB n points index 0
        tmp = [0, 0, r `div` occ , g `div` occ, b `div` occ]
    in tmp : calculateCentroid xs points index (n + 1)
