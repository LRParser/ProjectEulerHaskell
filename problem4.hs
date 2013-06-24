-- Find largest palindrome from product of two 3-digit number

threeDigitNums = [100..999]

allProds = [x*y | x <- threeDigitNums, y <- threeDigitNums]

-- From: http://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

firstAndLastSame n =
  let lengthDigits = length n
      firstDigit = n !! 0
      lastDigit = n !! (lengthDigits - 1)
      in firstDigit == lastDigit

removeFirstAndLast w =
  let wLen = length w
      numTake = wLen - 2
      numDrop = 1
      in take numTake (drop numDrop w)

--isPalin :: Integral [x], => x -> a
isPalin w
  | length w == 0 = True
  | length w == 1 = True
  | otherwise = firstAndLastSame w && isPalin (removeFirstAndLast w)

-- How to get filter isPalin allProds to pass compiler error?
allPalins = filter even allProds
maxPalin = maximum allPalins