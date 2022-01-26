module Jan1822 where

fac :: Int -> Int
fac n = if n==0 then 1 else n * fac (n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

deadband :: Float -> Float
deadband n = if (abs n) < 0.2 then 0 else n

main :: IO()

main = do
 print $ deadband 0.1
 print $  fac 3
 print $ 7
