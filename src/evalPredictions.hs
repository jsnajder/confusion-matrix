import Statistics.ConfusionMatrix
import System.Environment
import System.Console.ParseArgs
import System.IO
import Control.Applicative
import Data.Maybe
import Text.Printf
import Control.Monad

arg = [
  Arg 0 (Just 'p') Nothing 
    (argDataDefaulted "label" ArgtypeString "1")
    "positive class label (default=1)",
  Arg 1 (Just 'c') (Just "confusion-matrix")
    Nothing
    "print out confusion matrix",
  Arg 2 Nothing Nothing  (argDataRequired "filename" ArgtypeString)
    "system preditions",
  Arg 3 Nothing Nothing  (argDataRequired "filename" ArgtypeString)
    "gold preditions"]

-- todo: add multiclass

main = do
  args <- parseArgsIO ArgsComplete arg
  xs <- lines <$> readFile (fromJust $ getArg args 2)
  ys <- lines <$> readFile (fromJust $ getArg args 3)
  let l = fromMaybe "1" $ getArg args 0
      m = twoWay l $ evalPredictions xs ys
      a = accuracy m
      p = precision m
      r = recall m
      f = f1 m
  printf "Acc = %.3f, P = %.3f, R = %.3f, F1 = %.3f\n" a p r f
  when (gotArg args 1) (putStrLn $ show m)
 
