{-# LANGUAGE TypeApplications #-}
module NinetyNine where
import System.Random
import Control.Monad

-- chainIO' :: IO a -> (a -> IO b) -> IO b
-- chainIO :: (a -> IO b) -> IO a -> IO b
chainIO = (=<<) @IO
-- mapIO :: (a -> b) -> IO a -> IO b
mapIO = fmap @IO

randInt :: Int -> IO Int
randInt n = randomRIO (0, n)

randBool :: IO Bool
randBool = randomRIO (False, True)

randBoolAndInt :: Int -> IO (Int, Bool)
randBoolAndInt n = chainIO (\x -> go x randBool) (randInt n)
  where go :: Int -> IO Bool -> IO (Int, Bool)
        go y ib = mapIO (\a -> (y, a)) ib

-- rndSelect :: [a] -> Int -> IO [a]
 --rndSelect xs 

-- Problem 23
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []     = (Nothing, [])
removeAt i xs
  | i < 0 = (Nothing, xs)
removeAt 1 (x:xs) = (Just x, xs)
removeAt t (x:xs) = case removeAt (t - 1) xs of (maybeY, ys) -> (maybeY, x:ys)

rndSelectOne :: [a] -> IO (Maybe a, [a])
rndSelectOne xs = go (randomRIO (1, length xs)) xs
  where go :: IO Int -> [a] -> IO (Maybe a, [a])
        go ioidx ys = mapIO (\idx -> removeAt idx ys) ioidx

-- ^^ mapIO :: (Int -> (Maybe a, [a])) -> IO Int -> IO (Maybe a, [a])

-- Problem 24
diffSelect :: [a] -> Int -> IO [a]
diffSelect xs 0 = pure []
diffSelect xs n = chainIO go (rndSelectOne xs)
  where go :: (Maybe a, [a]) -> IO [a]
        go (Nothing, as) = pure []
        go (Just a, as)  = mapIO (\as' -> a:as') (diffSelect as (n - 1))

-- ^^ mapIO :: ([a] -> [a]) -> IO [a] -> IO [a]
-- ^^ chainIO :: ((Maybe a, [a]) -> IO [a]) -> IO (Maybe a, [a]) -> IO [a]


--    [IO a]   -->   IO [a]

unsafeRndSelect :: [a] -> IO a
unsafeRndSelect xs = mapIO (\i -> xs !! i) (randomRIO (0, (length xs - 1)))

--  [1,2,3]  -->    [IO r, IO r, IO r]   -->  IO [r, r, r]
--  [Int]    -->        [IO a]             -->   IO [a]

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = pure []
rndSelect _  0 = pure []
rndSelect xs n = sequenceA $ map (\_ -> unsafeRndSelect xs) [1..n]


rndSelectRep :: [a] -> Int -> IO [a]
rndSelectRep [] _ = pure []
rndSelectRep _  0 = pure []
rndSelectRep xs n = replicateM n (unsafeRndSelect xs)
