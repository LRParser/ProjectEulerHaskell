-- Starting with an infinite list of 3,5,7,9, etc., we find all elements
-- that have only 1 prime factor (i.e., themselves)
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

-- Gives the numbers contained in the passed list parameter that are factors of
-- the passed 'n' parameter (i.e., numbers in the list that evenly divide n)
-- requires that the "match list" is sorted
factorsOf n (p:ps)
  | p*p > n        = [n] -- if the current head of the match list squared is >n, that means it both can't be a factor, nor can any number after it 
  | n `mod` p == 0 = p : factorsOf (n `div` p) (p:ps)
  | otherwise      = factorsOf n ps

-- primeFactors gives the 
primeFactors n = factorsOf n primes

ans = last $ (primeFactors 317584931803)

