{-------------------------------------------------------------------------------

 Statistics.ConfusionMatrix
 Basic performance measures derived from a confusion matrix

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

-- TODO: conf. intervals, ...

module Statistics.ConfusionMatrix where

import Control.Applicative
import Data.List

data ConfMatrix a = CM {
  labels :: [a],
  matrix :: [[Int]] } deriving (Show,Read,Eq,Ord)

data BinaryConfMatrix = BCM {
  tp :: Int,
  fp :: Int,
  fn :: Int,
  tn :: Int } deriving (Show,Read,Eq,Ord)

-- confusion matrix with default category labels 1, 2, ...
confMatrix :: [[Int]] -> ConfMatrix Int
confMatrix xs
  | isMatrix xs && n == m && n > 1 = CM [1..n] xs
  | otherwise = error "confMatrix: not an NxN matrix (N>=2)"
  where (n,m) = matrixDim xs

binaryConfMatrix :: [[Int]] -> BinaryConfMatrix
binaryConfMatrix xs
  | isMatrix xs && n==m && n == 2 = twoWay 1 $ confMatrix xs
  | otherwise = error "binaryConfMatrix: not a 2x2 matrix"
  where (n,m) = matrixDim xs

isMatrix :: [[Int]] -> Bool
isMatrix [] = False
isMatrix xs@(x:_) = all ((==n) . length) xs
  where n = length x

matrixDim :: [[Int]] -> (Int,Int)
matrixDim xs@(x:_) = (length x,length xs)

-- takes two annotation lists and computes the confusion matrix
-- xs: gold list, ys: list to be evaluated
-- rows: outputs ys, columns: outputs xs (gold)
comparePredictions :: (Eq a) => [a] -> [a] -> ConfMatrix a
comparePredictions xs ys
  | length vs > 16 = error "comparePredictions: too many categories"
  | otherwise      = CM vs [[count v1 v2 | v1 <- vs] | v2 <- vs]
  where zs = zip xs ys
        n  = length zs
        vs = nub (xs ++ ys)
        count a b = length $ filter (\(x,y) -> x==a && y==b) zs

-- 'c' is the positive class
compareBinaryPredictions :: Eq a => a -> [a] -> [a] -> BinaryConfMatrix
compareBinaryPredictions c xs = twoWay c . comparePredictions xs
       
rowSums :: (Num a) => [[a]] -> [a]
rowSums = map sum

columnSums :: (Num a) => [[a]] -> [a]
columnSums = rowSums . transpose

tableSum :: (Num a) => [[a]] -> a
tableSum = sum . map sum

addCounts :: BinaryConfMatrix -> BinaryConfMatrix -> BinaryConfMatrix
addCounts (BCM tp1 fp1 fn1 tn1) (BCM tp2 fp2 fn2 tn2) =
  BCM (tp1+tp2) (fp1+fp2) (fn1+fn2) (tn1+tn2)
--sumTable (CM cs t1) (CM _ t2) = CM cs $ zipWith (zipWith (+)) t1 t2

twoWay :: (Eq a) => a -> ConfMatrix a -> BinaryConfMatrix
twoWay c (CM cs t) = case findIndex (==c) cs of
  Nothing -> error "twoWay: category does not exist"
  Just i  -> BCM tp fp fn tn
    where tp = t!!i!!i
          fp = sum (t!!i) - tp
          fn = sum ((transpose t)!!i) - tp
          tn = tableSum t - tp - fp - fn

accuracy :: BinaryConfMatrix -> Double
accuracy (BCM tp fp fn tn) = 
  realToFrac (tp + tn) / realToFrac (tp + fp + fn + tn)

precision :: BinaryConfMatrix -> Double
precision (BCM tp fp _ _) = 
  realToFrac tp / (realToFrac tp + realToFrac fp)

recall :: BinaryConfMatrix -> Double
recall (BCM tp _ fn _) = 
  realToFrac tp / (realToFrac tp + realToFrac fn)

sensitivity :: BinaryConfMatrix -> Double
sensitivity = recall

specificity :: BinaryConfMatrix -> Double
specificity (BCM _ fp fn tn) = 
  realToFrac tn / realToFrac (fp + fn)

-- parametrized F-measure 
-- b<1 emphasizes precision, b>1 emphasizes recall
fMeasure :: Double -> BinaryConfMatrix -> Double
fMeasure b t = (b^2 + 1) * p * r / (b^2 * p + r)
  where p = precision t
        r = recall t

f1 :: BinaryConfMatrix -> Double
f1 = fMeasure 1

microConfMatrix :: Eq a => ConfMatrix a -> BinaryConfMatrix
microConfMatrix cm@(CM cs _) = 
  foldr1 addCounts $ map (flip twoWay cm) cs

microFMeasure :: Eq a => Double -> ConfMatrix a -> Double
microFMeasure b = fMeasure b . microConfMatrix 

microF1 :: Eq a => ConfMatrix a -> Double
microF1 = microFMeasure 1

precisionOf :: Eq a => a -> ConfMatrix a -> Double
precisionOf c = precision . twoWay c

recallOf :: Eq a => a -> ConfMatrix a -> Double
recallOf c = recall . twoWay c

fMeasureOf :: Eq a => Double -> a -> ConfMatrix a -> Double
fMeasureOf b c = fMeasure b . twoWay c

f1Of :: Eq a => a -> ConfMatrix a -> Double
f1Of = fMeasureOf 1

macroPrecision :: Eq a => ConfMatrix a -> Double
macroPrecision cm@(CM cs _) = avg $ map (\c -> precisionOf c cm) cs

macroRecall :: Eq a => ConfMatrix a -> Double
macroRecall cm@(CM cs _) = avg $ map (\c -> recallOf c cm) cs

macroFMeasure :: Eq a => Double -> ConfMatrix a -> Double
macroFMeasure b cm@(CM cs _) = avg $ map (\c -> fMeasureOf b c cm) cs

macroF1 :: Eq a => ConfMatrix a -> Double
macroF1 = macroFMeasure 1

avg :: Fractional a => [a] -> a
avg xs = sum xs / (realToFrac $ length xs)

-- takes an NxN confusion matrix and computes the kappa 
-- coefficient and the standard error
kappa :: ConfMatrix a -> (Double,Double)
kappa (CM _ xss) = (k,se)
  where 
    ix  = [0..length xss-1]            -- 0..numberOfCategories
    n   = sum . map sum $ xss          -- sample size
    pss = map (map (\x -> realToFrac x / realToFrac n)) xss
    p1m = rowSums pss                  -- marginal probabilities (coder 1)
    p2m = columnSums pss               -- marginal probabilities (coder 2)
    pd  = diag pss                     -- diagonal probabilities
    ao  = sum pd                       -- observed agreement
    ae  = sum $ zipWith (*) p1m p2m    -- expeCMed agreement
    k   = (ao - ae) / (1 - ae)         -- kappa coefficient 
    se  = sqrt (seN / seD)             -- standard error
    seN = sum [pd!!i*((1-ae)-(p1m!!i+p2m!!i)*(1-ao))^2 | i <- ix] + (1-ao)^2 * 
          sum [(pss!!i)!!j*(p2m!!i+p1m!!j)^2 | i <- ix, j <- ix, i/=j] -
          (ao*ae - 2*ae + ao)^2
    seD = (realToFrac n)*(1-ae)^4
    diag xss = [xs!!i | (xs,i) <- zip xss [0..]]

