module Main (main) where
import Lib (processInput, readCoordinatePair, parseCommandLineArguments)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    args <- getArgs
    let config = parseCommandLineArguments args
    putStrLn "Enter frist two dots (x y):"
    hFlush stdout
    firstPoint <- readCoordinatePair
    hFlush stdout
    secondPoint <- readCoordinatePair
    processInput config [firstPoint, secondPoint]