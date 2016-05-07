import Data.Array
import Control.Monad.State
import System.Random
import System.Environment (getArgs)

type FastModel = Array Int (String,[(Int,Int)])

runModel :: Int -> FastModel -> State StdGen [String]
runModel len model = do
  startIx <- state $ randomR (1,snd $ bounds model)   
  let (str,next) = model ! startIx
  reverse . fst <$> foldM genOneWord ([str],next) [1..len] 
  where
    genOneWord (strs,next) _ = do
      succIx <- state $ randomR (0, sum (map fst next) - 1)
      let newIx = indexFreqs succIx next
      let (newStr, newNext) = model ! newIx
      return (newStr : strs, newNext)
    indexFreqs i ((f,s):xs) 
      | i < f = s
      | otherwise = indexFreqs (i-f) xs

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
  iter x (y:ys) 
    | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
    | otherwise                   = iter (x ++ " " ++ y) ys
  iter x [] = x ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  let len = read $ head args
  let inFile = "sokal.model"
  modelStrs <- lines <$> readFile inFile 
  let modelTpls = read <$> modelStrs
  let upBound = length modelTpls
  let fastModel = array (1,upBound) (zip [1..] modelTpls)
  gen <- getStdGen
  let output = evalState (runModel len fastModel) gen
  let cleaned = tail $ dropWhile ((/= '.') . last) output
  putStr $ linefill 72 cleaned
