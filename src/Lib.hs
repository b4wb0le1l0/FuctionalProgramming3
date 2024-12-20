{-# LANGUAGE GADTs #-}

module Lib
  ( polynomialInterpolation,
    lagrangeInterpolation,
    linearInterpolation,
    processInput,
    parseCommandLineArguments,
    readCoordinatePair,
  )
where

import Control.Monad (when)
import Data.Maybe (mapMaybe)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Configuration where
  Configuration ::
    {interpolationMethods :: [String], stepSize :: Double} ->
    Configuration

polynomialInterpolation :: Double -> [(Double, Double)] -> Double
polynomialInterpolation x points = sum [y_i * product [(x - x_j) / (x_i - x_j) | (x_j, _) <- points, x_i /= x_j] | (x_i, y_i) <- points]

linearInterpolation :: Double -> (Double, Double) -> (Double, Double) -> [Double]
linearInterpolation step (x1, y1) (x2, y2) =
  let xs = takeWhile (< x2 + step) [x1, x1 + step ..]
   in map (\x -> y1 + (y2 - y1) * (x - x1) / (x2 - x1)) xs

lagrangeInterpolation :: Double -> [(Double, Double)] -> [Double]
lagrangeInterpolation step points =
  let xs = [fst (head points), fst (head points) + step .. fst (last points) + step]
   in map (`polynomialInterpolation` points) xs

displayResults :: [Double] -> IO ()
displayResults = mapM_ (putStr . printf "  %.2f")

processInput :: Configuration -> [(Double, Double)] -> IO ()
processInput config points = do
  let numberOfPoints = length points

  when (numberOfPoints >= 2) $ do
    let recentPairs = take 2 $ reverse points
    let (x1, y1) = recentPairs !! 1
    let (x2, y2) = head recentPairs
    let xs_line = [x1, x1 + 1 .. x2]
    putStrLn $ "Linear from " ++ show (fst (head recentPairs)) ++ " in increments " ++ show (stepSize config) ++ " covering all entered values of x " ++ show (fst (head recentPairs)) ++ " < " ++ show (last xs_line)

    putStrLn $ unwords (map (printf "  %.2f") xs_line)
    let lineResult = linearInterpolation (stepSize config) (recentPairs !! 1) (head recentPairs)
    displayResults lineResult

  when (numberOfPoints >= 4) $ do
    let recentPairs = take 4 $ reverse points
    let (x1, y1) = recentPairs !! 1
    let (x2, y2) = head recentPairs
    let xs_line = [x1, x1 + 1 .. x2]
    putStrLn $ "\nLagrange from " ++ show (fst (last recentPairs)) ++ " in increments " ++ show (stepSize config) ++ " covering all entered values of x " ++ show (fst (head recentPairs)) ++ " < " ++ show (last xs_line)

    putStrLn $ unwords (map (printf "  %.2f") xs_line)
    let polynomialResult = lagrangeInterpolation (stepSize config) (reverse recentPairs)
    displayResults polynomialResult

  putStrLn "\nEnter new dot (x y):"
  hFlush stdout
  newPair <- readCoordinatePair
  processInput config (points ++ [newPair])

parseCommandLineArguments :: [String] -> Configuration
parseCommandLineArguments args =
  Configuration
    { interpolationMethods = filter (`elem` ["linear", "lagrange"]) args,
      stepSize = case mapMaybe readMaybe args of
        (x : _) -> x
        [] -> 1.0
    }

parseInputLine :: String -> Maybe (Double, Double)
parseInputLine line = case words (map replaceComma line) of
  [xString, yString] -> do
    x <- readMaybe xString
    y <- readMaybe yString
    return (x, y)
  _ -> Nothing
  where
    replaceComma ',' = ' '
    replaceComma c = c

readCoordinatePair :: IO (Double, Double)
readCoordinatePair = do
  line <- getLine
  case parseInputLine line of
    Just p -> return p
    Nothing -> putStrLn "Incorrect enter, try again:" >> readCoordinatePair
