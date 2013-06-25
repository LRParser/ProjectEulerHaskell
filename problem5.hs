-- Smallest # divisble by 1 thru 20

divisors = [1..20]
allNums = [1..3]
isDivisible x y = (x `mod` y) == 0 && (x `div` y) == 1
