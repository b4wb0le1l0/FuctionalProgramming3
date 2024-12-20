# Лабораторная работа № 3

Аганин Егор Владимирович

## Описание

Данная программа реализует алгоритмы линейной интерполяции и интерполяции Лагранжа. Она принимает входные данные в текстовом формате (csv) и выводит интерполированные значения в стандартный вывод в потоковом режиме.

## Требования к выполняемой работе

## Требования

- Реализация линейной интерполяции (отрезками).
- Возможность задавать параметры интерполяции и формат вывода через аргументы командной строки:
  - Используемые алгоритмы (можно задать несколько).
  - Частота дискретизации результирующих данных.
- Входные данные должны подаваться в текстовом формате, отсортированные по возрастанию `x`.
- Программа должна работать в потоковом режиме, обрабатывая данные по мере их поступления.

## Архитектура

Приложение организовано следующим образом:

```
+---------------------------+
| обработка входного потока |
+---------------------------+
|
| поток / список / последовательность точек
v
+-----------------------+      +------------------------------+
| алгоритм интерполяции |<-----| генератор точек, для которых |
+-----------------------+      | необходимо вычислить         |
|                      | промежуточные значения       |
v                      +------------------------------+
+------------------------+
| печать выходных данных |
+------------------------+
```

## Реализация

- Методы интерполяции и другие вспомогательные функции реализованы в файле [Lib.hs](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/src/Lib.hs)

### Линейная интерполяция

```Haskell
linearInterpolation :: Double -> (Double, Double) -> (Double, Double) -> [Double]
linearInterpolation step (x1, y1) (x2, y2) =
  let xs = takeWhile (< x2 + step) [x1, x1 + step ..]
   in map (\x -> y1 + (y2 - y1) * (x - x1) / (x2 - x1)) xs
```

### Интерполяция Лагранжа

```Haskell
lagrangeInterpolation :: Double -> [(Double, Double)] -> [Double]
lagrangeInterpolation step points =
  let xs = [fst (head points), fst (head points) + step .. fst (last points) + step]
   in map (`polynomialInterpolation` points) xs
```

### Генерация точек

```Haskell
polynomialInterpolation :: Double -> [(Double, Double)] -> Double
polynomialInterpolation x points = sum [y_i * product [(x - x_j) / (x_i - x_j) | (x_j, _) <- points, x_i /= x_j] | (x_i, y_i) <- points]
```

### Процесс интерполяции

```Haskell
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
```

### [Main.hs](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/src/Main.hs)

```Haskell
module Main (main) where

import Lib (parseCommandLineArguments, processInput, readCoordinatePair)
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
```

### Тесты реализованы в файле [Spec.hs](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/src/Spec.hs)

```Haskell
module Main
  ( main,
    testLagrange,
    testLinearInterpolation,
  )
where

import Lib
import Test.HUnit

testLagrange :: Test
testLagrange =
  TestCase
    ( do
        let points = [(1, 1), (2, 4), (3, 9)]
        assertEqual "Lagrange interpolation at x=2 should be 4" 4.0 (polynomialInterpolation 2 points)
        assertEqual "Lagrange interpolation at x=1.5 should be 2.25" 2.25 (polynomialInterpolation 1.5 points)
    )

testLinearInterpolation :: Test
testLinearInterpolation =
  TestCase
    ( do
        let step = 0.5
        let (x1, y1) = (1, 1)
        let (x2, y2) = (3, 3)
        let result = linearInterpolation step (x1, y1) (x2, y2)
        assertEqual "Should return interpolated values" [1.0, 1.5, 2.0, 2.5, 3.0] result
    )

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ testLagrange,
          testLinearInterpolation
        ]
  return ()
```

## Результаты тестирования и работы программы

### Обработка данных из файла

[Скачать входные данные](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/input.txt)

![Обработка данных из файла](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/fromFileToOut.png)

### Результат тестов

![Результат тестов](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/tests.png)

### Работа программы с "классическим" IO

![Работа программы с "классическим" IO](https://github.com/b4wb0le1l0/FuctionalProgramming3/blob/main/ClassicIO.png)
