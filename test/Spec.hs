
module Main (main, testLagrange,
  testLinearInterpolation) where
import Test.HUnit
import Lib

testLagrange :: Test
testLagrange = TestCase (do
    let points = [(1, 1), (2, 4), (3, 9)]
    assertEqual "Lagrange interpolation at x=2 should be 4" 4.0 (polynomialInterpolation 2 points)
    assertEqual "Lagrange interpolation at x=1.5 should be 2.25" 2.25 (polynomialInterpolation 1.5 points))

testLinearInterpolation :: Test
testLinearInterpolation = TestCase (do
    let step = 0.5
    let (x1, y1) = (1, 1)
    let (x2, y2) = (3, 3)
    let result = linearInterpolation step (x1, y1) (x2, y2)
    assertEqual "Should return interpolated values" [1.0, 1.5, 2.0, 2.5, 3.0] result)

main :: IO ()
main = do
  _ <-
    runTestTT $
        TestList
            [ 
             testLagrange
            , testLinearInterpolation]
  return ()