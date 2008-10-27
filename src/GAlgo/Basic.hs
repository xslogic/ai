module GAlgo.Basic where

import Random
import Data.Map as M
import Prelude as P



getRandomList :: Int -> IO [Bool]
getRandomList num = mapM (\_ -> getStdRandom (randomR (True, False))) [1..num]

getRndListOfList :: Int -> Int -> IO [[Bool]]
getRndListOfList num1 num2 = mapM (\_ -> getRandomList num1) [1..num2]




crossOver :: Float -> [Bool] -> [Bool] -> IO ([Bool], [Bool])
crossOver cRate l1 l2 = do
  i <- getStdRandom $ randomR $ (0.0, 1.0)
  if (i <= cRate)
     then crossOver' l1 l2
     else return (l1, l2)

crossOver' :: [Bool] -> [Bool] -> IO ([Bool], [Bool])
crossOver' l1 l2 = do
  let l = length l1
  i <- getStdRandom $ randomR $ (1, l)
  return $ crossOver'' i l l1 l2 [] []


crossOver'' :: Int -> Int -> [Bool] -> [Bool] -> [Bool] -> [Bool] -> ([Bool], [Bool])
crossOver'' _ _ [] _ xs' ys' = (reverse xs', reverse ys')
crossOver'' _ _ _ [] xs' ys' = (reverse xs', reverse ys')
crossOver'' n l (x:xs) (y:ys) xs' ys' | (n < l)   = crossOver'' n (l - 1) xs ys (y:xs') (x:ys')
                                      | otherwise = crossOver'' n (l - 1) xs ys (x:xs') (y:ys')


insertSort :: Float -> b -> [(Float, b)] -> [(Float, b)]
insertSort k v [] = [(k, v)]
insertSort k v ((k', v'):xs) | (k >= k')   = (k, v):(k', v'):xs
                             | otherwise  = (k', v'):(insertSort k v xs)



mutate :: [Bool] -> Float -> IO ([Bool])
mutate l p = mapM (flip' p) l


flip' :: Float -> Bool -> IO (Bool)
flip' p b = do
  x <- getStdRandom $ randomR (0.0, 1.0)
  if (x <= p)
     then return (not b)
     else return b



removeRandomFromMap :: Float -> [(Float, a)] -> IO (Float, a, [(Float, a)])
removeRandomFromMap sum m = do
  i <- getStdRandom $ randomR $ (0.0, 1.0)
{--
  putStrLn $ "Size of map : " ++ (show (length m))
               ++ " Sum : " ++ (show sum)
               ++ " i : " ++ (show i)
               ++ " val : " ++ (show (i * sum))
-}
  return $ removeFromMap (i * sum) m 


{--
merge' :: Float -> Map Float a -> Map Float a -> Map Float a
merge' k mL mG | (0 == size mL)&&(0 /= size mG)  = let mG' = mapKeys (\key -> key - k) mG
                                                       (maxKey, _) = findMax mG'
                                                   in mapKeys (\key -> (key / maxKey)) mG'                          
               | (0 /= size mL)&&(0 == size mG)  = let (maxKey, _) = findMax mL
                                                   in mapKeys (\key -> (key / maxKey)) mL
               | (0 /= size mL)&&(0 /= size mG)  = let (mK, _) = findMax mL
                                                       k' = k - mK
                                                       l  = (k' * mK) / (1 - k + mK)
                                                       g  = k' - l
                                                       mG' = merge' g M.empty mG
                                                       mL' = mapKeys (\key -> ((key * (mK + l)) / mK)) mL
                                                   in union mL' mG'
               | otherwise                       = M.empty

-}



removeFromMap :: Float -> [(Float, a)] -> (Float, a, [(Float, a)])
removeFromMap weight m = removeFromMap' weight 0 m []


removeFromMap' :: Float -> Float -> [(Float, a)] -> [(Float, a)] -> (Float, a, [(Float, a)])
removeFromMap' w cw ((s', a'):m) m' = if (cw + s'>= w)
                                         then (s', a', (reverse m') ++ m)
                                         else removeFromMap' w (cw + s') m ((s', a'):m')
{--
removeFromMap weight m = let (m'', m') = partitionWithKey (\k v -> (k < weight)) m
                             ((key, val), rest) = deleteFindMin m'
                         in (val, merge' key m'' rest)
-}



dist1 func = foldl (\(s', l') x -> (s' + (func x), insertSort (func x) x l')) (0.0, [])
{-- dist2 sum = foldl (\(cs, cl) (s, ls) -> (cs + s, ((cs + s) / sum, ls):cl)) (0.0, [])
-}

distribute :: (a -> Float) -> [a] -> (Float, [(Float, a)])
distribute func xs = dist1 func  xs 



