{-------------------------------------------------------------------------------

 Statistics.ConfusionMatrix
 Basic performance measures derived from a confusion matrix

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

-- TODO: conf. intervals, ...

module Statistics.ConfusionMatrix (
  ConfMatrix,
  BinaryConfMatrix,
  confMatrix,
  binaryConfMatrix,
  numInstances,
  evalPredictions,
  evalBinaryPredictions,
  majorityClassifier,
  twoWay,
  accuracy,
  precision,
  recall,
  sensitivity,
  specificity,
  fMeasure,
  f1,
  microConfMatrix,
  microAccuracy,
  microPrecision,
  microRecall,
  microSensitivity,
  microSpecificity,
  microFMeasure,
  microF1,
  precisionOf,
  recallOf,
  sensitivityOf,
  specificityOf,
  fMeasureOf,
  f1Of,
  macroAccuracy,
  macroPrecision,
  macroRecall,
  macroSensitivity,
  macroSpecificity,
  macroFMeasure,
  macroF1,
  kappa) where

import Data.List
import Data.Ord

-- | Multiclass confusion matrix.
data ConfMatrix a = CM {
  labels :: [a],
  matrix :: [[Int]] } deriving (Show,Read,Eq,Ord)

-- | Binary confusion matrix.
data BinaryConfMatrix = BCM {
  tp :: Int,
  fp :: Int,
  fn :: Int,
  tn :: Int } deriving (Show,Read,Eq,Ord)

-- | Builds a multiclass confusion matrix. Indices @1@, @2@, etc. are used as
-- category labels.
confMatrix :: [[Int]] -> ConfMatrix Int
confMatrix xs
  | isMatrix xs && n == m && n > 1 = CM [1..n] xs
  | otherwise = error "confMatrix: not an NxN matrix (N>=2)"
  where (n,m) = matrixDim xs

-- | Builds a binary confusion matrix from a list @[[TP,FN],[FP,TN]]@, where
-- @TP@, @FN@, @FP@, and @TN@ are the true positives, the false positives, the
-- false negatives, and the true negatives, respectively.
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

-- | Computes the confusion matrix by comparing model predictions (first list) 
-- against gold predictions (second list).
-- @
-- evalPredictions "aabbcc" "abbbcc" == confusionMatrix
-- [[1,0,0],[1,2,0],[0,0,2]]
-- @
evalPredictions :: (Eq a, Ord a) => [a] -> [a] -> ConfMatrix a
evalPredictions xs ys
  | length vs > 16 = error "comparePredictions: too many categories"
  | otherwise      = CM vs [[count v1 v2 | v1 <- vs] | v2 <- vs]
  where zs = zip xs ys
        n  = length zs
        vs = sort $ nub (xs ++ ys)
        count a b = length $ filter (\(x,y) -> x==a && y==b) zs

-- | Same as 'evalPredictions' but for binary (boolean) predictions. 
-- 'True' is considered as positive class.
-- @
-- evalPredictions [True,True,False] [True,False,False] == binaryConfMatrix
-- [[1,0],[1,1]]
-- @
evalBinaryPredictions :: [Bool] -> [Bool] -> BinaryConfMatrix
evalBinaryPredictions xs = twoWay True . evalPredictions xs

majorityClassifier :: ConfMatrix a -> ConfMatrix a
majorityClassifier (CM cs m) = 
  CM cs $ replicate i zs ++ [xs] ++ replicate (n-i-1) zs
  where xs = columnSums m
        i  = fst . maximumBy (comparing snd) $ zip [0..] xs
        zs = replicate n 0
        (n,_) = matrixDim m

numInstances :: ConfMatrix a -> Int
numInstances (CM _ m) = matrixSum m
 
rowSums :: (Num a) => [[a]] -> [a]
rowSums = map sum

columnSums :: (Num a) => [[a]] -> [a]
columnSums = rowSums . transpose

matrixSum :: (Num a) => [[a]] -> a
matrixSum = sum . map sum

addCounts :: BinaryConfMatrix -> BinaryConfMatrix -> BinaryConfMatrix
addCounts (BCM tp1 fp1 fn1 tn1) (BCM tp2 fp2 fn2 tn2) =
  BCM (tp1+tp2) (fp1+fp2) (fn1+fn2) (tn1+tn2)

twoWay :: (Eq a) => a -> ConfMatrix a -> BinaryConfMatrix
twoWay c (CM cs m) = case findIndex (==c) cs of
  Nothing -> error "twoWay: category does not exist"
  Just i  -> BCM tp fp fn tn
    where tp = m!!i!!i
          fn = sum (m!!i) - tp
          fp = sum ((transpose m)!!i) - tp
          tn = matrixSum m - tp - fp - fn

accuracy :: BinaryConfMatrix -> Double
accuracy (BCM tp fp fn tn) = 
  realToFrac (tp + tn) / realToFrac (tp + fp + fn + tn)

precision :: BinaryConfMatrix -> Double
precision (BCM tp fp _ _) = 
  realToFrac tp / (realToFrac tp + realToFrac fp)

recall :: BinaryConfMatrix -> Double
recall (BCM tp _ fn _) = 
  realToFrac tp / (realToFrac tp + realToFrac fn)

-- equals recall
sensitivity :: BinaryConfMatrix -> Double
sensitivity = recall

specificity :: BinaryConfMatrix -> Double
specificity (BCM _ fp fn tn) = 
  realToFrac tn / realToFrac (fp + tn)

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

microAccuracy :: Eq a => ConfMatrix a -> Double
microAccuracy = accuracy . microConfMatrix

microPrecision :: Eq a => ConfMatrix a -> Double
microPrecision = precision . microConfMatrix

-- equals microPrecision
microRecall :: Eq a => ConfMatrix a -> Double
microRecall = recall . microConfMatrix

-- equals microPrecision
microSensitivity :: Eq a => ConfMatrix a -> Double
microSensitivity = sensitivity . microConfMatrix

-- equals microPrecision
microSpecificity :: Eq a => ConfMatrix a -> Double
microSpecificity = specificity . microConfMatrix

-- equals microPrecision (for all values of b)
microFMeasure :: Eq a => Double -> ConfMatrix a -> Double
microFMeasure b = fMeasure b . microConfMatrix 

-- equals microPrecision
microF1 :: Eq a => ConfMatrix a -> Double
microF1 = microFMeasure 1

accuracyOf :: Eq a => a -> ConfMatrix a -> Double
accuracyOf c = accuracy . twoWay c

precisionOf :: Eq a => a -> ConfMatrix a -> Double
precisionOf c = precision . twoWay c

recallOf :: Eq a => a -> ConfMatrix a -> Double
recallOf c = recall . twoWay c

sensitivityOf :: Eq a => a -> ConfMatrix a -> Double
sensitivityOf c = sensitivity . twoWay c

specificityOf :: Eq a => a -> ConfMatrix a -> Double
specificityOf c = specificity . twoWay c

fMeasureOf :: Eq a => Double -> a -> ConfMatrix a -> Double
fMeasureOf b c = fMeasure b . twoWay c

f1Of :: Eq a => a -> ConfMatrix a -> Double
f1Of = fMeasureOf 1

-- equals microAccuracy
macroAccuracy :: Eq a => ConfMatrix a -> Double
macroAccuracy cm@(CM cs _) = avg $ map (flip accuracyOf cm) cs

macroPrecision :: Eq a => ConfMatrix a -> Double
macroPrecision cm@(CM cs _) = avg $ map (flip precisionOf cm) cs

macroRecall :: Eq a => ConfMatrix a -> Double
macroRecall cm@(CM cs _) = avg $ map (flip recallOf cm) cs

macroSensitivity :: Eq a => ConfMatrix a -> Double
macroSensitivity cm@(CM cs _) = avg $ map (flip sensitivityOf cm) cs

macroSpecificity :: Eq a => ConfMatrix a -> Double
macroSpecificity cm@(CM cs _) = avg $ map (flip specificityOf cm) cs

macroFMeasure :: Eq a => Double -> ConfMatrix a -> Double
macroFMeasure b cm@(CM cs _) = avg $ map (\c -> fMeasureOf b c cm) cs

macroF1 :: Eq a => ConfMatrix a -> Double
macroF1 = macroFMeasure 1

avg :: Fractional a => [a] -> a
avg xs = sum xs / (realToFrac $ length xs)

-- | Takes a confusion matrix and computes the kappa coefficient and 
-- the standard error.
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

