-- Smallest # divisble by 1 thru 20

-- Native solution
divisors = [1..20]
allNums = [1..]
isDivisible x y = (x `mod` y) == 0
allDivisible x = not (elem False (map (isDivisible x) divisors))
firstDivisible = head (filter allDivisible allNums)

-- Quick solution

ans = foldr1 (*) divisors