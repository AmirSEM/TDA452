{- Lab 1
   Date: 2024-10-11
   Authors: Johan Larsson johx@student.chalmers.se
   Lab group: 68
 -}

import Test.QuickCheck

--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1 
-- one could use the master theorem to analyze the number of recursive steps
-- taken here but it is clear that there will be k + 1 steps taken to compute


-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k
   | k < 0 = error "power: negative argument"
power1 n 0 = product []  -- wasnt needed
power1 n k = product [n | _ <- [1..k]]

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n k
   | k == 0 = 1
   | even k = power2 (n * n) (div k 2)
   -- it is tempting to do "even k = power2 (power2 n 2) (div k 2)" but
   -- then we would need another base case for when k = 2
   | odd k = n * power2 n (k - 1)
   | otherwise = error "power: negative argument"

-- D -------------------------
{- 

<Describe your test cases here>
We have definded the power functions to take Integer arguments so it would
be wise to test some clear non-integers:
   power pi 10
   power 3 pi
   power "as" 3
   power 2 (-1)
   power 'f' 2
   power [3] 4

Some other tests that should terminate correctly, by definition, are:
   power 2 3
   power 2 0 
   power 2 1
   power 1 4
   power 5 7
   power (-4) 5
   power (-4) 2


 -}
tests = [[2,3],[2,0],[2,1],[1,4],[5,7],[-4,5],[-4,2]]

tests1 = [(2,3),(2,0),(2,1),(1,4),(5,7),(-4,5),(-4,2)] --Could use list of tuples instead of lists of lists

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k
   | (power n k) /= (power1 n k) = False
   | (power n k) /= (power2 n k) = False
   | otherwise = True
   -- This works by transitivity, we only need to make 2 comparisons since if
   -- a == b and b == c then a == c we return true, in one differs we return false

--
powerTest :: Bool
powerTest = and [prop_powers (t!!0) (t!!1) | t <- tests]
-- powerTest = and [prop_powers a b | (a,b) <- tests1] --this like look cleaner than the submission above


--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)