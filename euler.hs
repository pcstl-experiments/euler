import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (foldl', maximumBy)
import Data.Array (listArray, (!))

isqrt = ceiling . sqrt . fromIntegral
isqrtDown = floor . sqrt . fromIntegral

primes :: [Integer]
primes = 2 : 3 : 5 : (filter isPrime [7..])
  where isPrime n = null (smallestPrimeFactors n)

smallestPrimeFactors :: Integer -> [Integer]
smallestPrimeFactors n = filter (\x -> n `mod` x == 0) (potentialFactors n)
  where potentialFactors n = takeWhile (<= isqrt n) primes
        isqrt              = ceiling . sqrt . fromIntegral

uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors n = filter (\x -> n `mod` x == 0) (potentialFactors n)
  where potentialFactors n = takeWhile (<= n `div` 2) primes

allPrimeFactors :: Integer -> [Integer]
allPrimeFactors n = if null factors && n > 1 then [n] else factors
  where factors                   = replicateFactors n uniqueFactors
        uniqueFactors             = uniquePrimeFactors n
        replicateFactors 0 _      = []
        replicateFactors _ []     = []
        replicateFactors n fl@(f:fs)
          | n `mod` f == 0 = f : (replicateFactors (n `div` f) fl)
          | otherwise      = replicateFactors n fs

countedPrimeFactors :: Integer -> Map.Map Integer Int
countedPrimeFactors n = Map.fromList (counts factors)
  where factors         = allPrimeFactors n
        counts []       = []
        counts l@(x:xs) =
          let (prefix, suffix) = break (/= x) l
          in (x, length prefix) : counts suffix

minimalCommonFactors :: [Integer]-> Map.Map Integer Int
minimalCommonFactors nums =
  let factorCounts = map countedPrimeFactors nums
  in foldr (Map.unionWith max) Map.empty factorCounts

factorCount :: Integer -> Integer
factorCount num =
  let primeFactorCounts = Map.elems $ countedPrimeFactors num
  in product ((+1) <$> ((fromIntegral <$> primeFactorCounts) :: [Integer]))

smallestEvenlyDivided :: [Integer] -> Integer
smallestEvenlyDivided nums = product commonFactors
  where commonFactors = concatMap (uncurry (flip replicate)) factorList
        factorList    = Map.toList (minimalCommonFactors nums)

reverseNumber :: Integer -> Integer
reverseNumber n =
  let digits = reverseDigits n
  in sum $ zipWith (\x y -> x * (10^(y-1))) digits $ countDown (length digits)
  where
    reverseDigits 0    = []
    reverseDigits n    = n `mod` 10 : reverseDigits (n `div` 10)
    countDown n = [n, n-1 .. 1]

isPalindrome :: Integer -> Bool
isPalindrome x = x == reverseNumber x

fastUniqueElements :: Ord a => [a] -> [a]
fastUniqueElements = Set.toList . Set.fromList

-- Lines in 2D matrices
selectIndices :: ((Int, Int) -> (Int, Int)) -> Int -> (Int, Int) -> [(Int, Int)]
selectIndices nextIndex len start = selectIndices' start 0 [start]
  where selectIndices' currentIndex currentLen acc
          | currentLen == len - 1 = acc
          | otherwise
          = let next = nextIndex currentIndex
            in selectIndices' next (currentLen + 1) ((nextIndex currentIndex) : acc)

lineVert :: Int -> (Int, Int) -> [(Int, Int)]
lineVert = selectIndices (\(x, y) -> (x + 1, y))

lineHoriz :: Int -> (Int, Int) -> [(Int, Int)]
lineHoriz = selectIndices (\(x, y) -> (x, y + 1))

lineDiagRight :: Int -> (Int, Int) -> [(Int, Int)]
lineDiagRight = selectIndices (\(x, y) -> (x + 1, y + 1))

lineDiagLeft :: Int -> (Int, Int) -> [(Int, Int)]
lineDiagLeft = selectIndices (\(x, y) -> (x + 1, y - 1))

allProducts :: [Integer] -> [Integer]
allProducts l = fastUniqueElements $ allProducts' l
  where allProducts' []     = []
        allProducts' [x]    = [x]
        allProducts' (n:ns) =
          let rest = allProducts ns
          in [n] ++ rest ++ ((n *) <$> rest) 

reverseDigits :: Integer -> [Integer]
reverseDigits n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : (reverseDigits $ n `div` 10)

digits :: Integer -> [Integer]
digits = reverse . reverseDigits

-- TODO: Should use state monad
memoizeWithMap :: Ord k => (k -> a) -> k -> Map.Map k a -> (a, Map.Map k a)
memoizeWithMap f x map = case Map.lookup x map of
  Just result -> (result, map)
  Nothing ->
    let result = f x
    in (result, Map.insert x result map)
