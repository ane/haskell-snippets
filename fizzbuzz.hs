-- The Fizzbuzz programming skill test
-- (C) ane 2011 <ane@iki.fi>

module Main where

fizzBuzzes :: [Either String Int]
fizzBuzzes = map fizzBuzz [1..]
  where
    fizzBuzz x | x `rem` 3 == 0 && x `rem` 5 == 0 = Left "FizzBuzz"
               | x `rem` 3 == 0 = Left "Fizz"
               | x `rem` 5 == 0 = Left "Buzz"
               | otherwise = Right x

printFizzBuzz :: Either String Int -> IO ()
printFizzBuzz fb = case fb of
                     Left s -> putStrLn s
                     Right x -> print x

main = mapM printFizzBuzz (take 50 fizzBuzzes)
