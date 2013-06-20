--Naive impl
fib' 0 = 0
fib' 1 = 1
fib' x = fib'(x-1) + fib'(x-2)
fibs' = map fib' [1..5]

--Optimized
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

evenFibs = [x | x <- takeWhile (<= 4000000) fibs, even x]
sumEvenFibs = sum evenFibs