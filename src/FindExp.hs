module Main where

import GAlgo.Basic
import Prelude as P
import Data.Map as M


lst = zip [0..] "0123456789+-*/"
operMap = M.fromList [('+', (+)), ('-', (-)), ('*', (*)), ('/', (/))]


parse :: [Bool] -> String
parse [] = ""
parse (w:x:y:z:xs) = let conv = \x -> if (x) then 1 else 0
                         (w':x':y':z':_) = P.map conv (w:x:y:z:[])
                         num = (w' * 8) + (x' * 4) + (y' * 2) + z' in
                     case (P.lookup num lst) of
                       Nothing -> "" ++ (parse xs)
                       Just n -> n:(parse xs)

eval :: Char -> Float -> Float -> Float
eval oper n1 n2 = case (M.lookup oper operMap) of
                    Nothing -> 200
                    Just f -> f n1 n2

               
evalString :: String -> Float
evalString s = evalString' 0 '+' s


evalString' :: Float -> Char -> String -> Float
evalString' old _ [] = old 
evalString' old pre (c:cs) | (elem pre "+-*/") && (elem c "0123456789") = evalNum
                           | (elem pre "0123456789") && (elem c "+-*/") = evalOper
                           | otherwise = (1 / 0)
    where evalNum  = evalString' (eval pre old (read (c:""))::Float) c cs
          evalOper = if (0 == length cs)
                 then (1 / 0)
                 else evalString' old c cs 


score :: Float -> Float -> Float
score exp act = (1 / abs (exp - act))



iterate' :: Int -> Float -> Float -> Float -> [[Bool]] -> [[Bool]] -> IO [[Bool]]
iterate' 0 _ _ _ _ p0' = return p0'
iterate' pop cRate mRate val p0 p0' = do
  let (s, m) = distribute (func val) p0
  {-- putStrLn $ "Size of map : " ++ (show (length m)) -}
  (xScore, x, m') <- removeRandomFromMap s m
  (_, y, _) <- removeRandomFromMap (s - xScore) m'
  (l1, l2) <- crossOver cRate x y
  l1' <- mutate l1 mRate
  l2' <- mutate l2 mRate
  iterate' (pop - 2) cRate mRate val p0 (l1':l2':p0')


func val = (score val) . evalString . parse

max' val c@(cs, g') g = let s = func val g
                        in if (s > cs)
                            then (s, g)
                            else (cs, g')


{-- iterateN' 0 _ _ _ _ p0 = return p0 -}
iterateN' :: Int -> Int -> Float -> Float -> Float -> [[Bool]] -> IO [[Bool]]
iterateN' n pop cRate mRate val p0 = do
  let (scoreMax, g) = foldl (max' val) (0.0, []) p0
  putStrLn $ "Max Score : " ++ (show scoreMax) 
               ++ ", String : " ++ (show (parse g)) 
               ++ ", Val : " ++ (show $ (evalString . parse) g) 
               ++ ", Iter : " ++ (show n)
  p0' <- iterate' pop cRate mRate val p0 []
  if (scoreMax == (1/0))
     then return p0'
     else iterateN' (n + 1) pop cRate mRate val p0'
  

{-- mutateRate -> crossoverRate -> chromosomeLength -> population -> numIter -> Val -> IO (BestAnswer) -}
findAnswer :: Float -> Float -> Int -> Int -> Float -> IO ()
findAnswer mRate cRate cLen pop val = do
  p0 <- getRndListOfList cLen pop
  pFinal <- iterateN' 0 pop cRate mRate val p0
  print "Done"

main = findAnswer 0.01 0.7 20 1000 45
