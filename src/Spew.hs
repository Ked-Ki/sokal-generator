import Data.Array
import Control.Monad.State
import Control.Monad.Writer
import System.Random
import System.Environment (getArgs)

type FastModel = Array Int (String,[(Int,Int)])

data InnerState = InnerState { stdGen :: StdGen, curSuccs :: [(Int,Int)] }
type ModelState = WriterT [String] (State InnerState) ()

runModel :: Int -> FastModel -> ModelState
runModel len model = do
  startIx <- state $ randomR' (1,snd $ bounds model)
  let (str,next) = model ! startIx
  tell [str]
  putSuccs next
  replicateM_ len genWord
  where
    genWord = do
      next <- getSuccs
      succIx <- state $ randomR' (0, sum (map fst next) - 1)
      let newIx = indexFreqs succIx next
      let (newStr, newNext) = model ! newIx
      tell [newStr]
      putSuccs newNext

    indexFreqs i ((f,s):xs) 
      | i < f = s
      | otherwise = indexFreqs (i-f) xs

    -- State helpers
    randomR' :: Random a => (a,a) -> InnerState -> (a, InnerState)
    randomR' bs st = (val, st { stdGen = gen' })
      where
        (val, gen') = randomR bs (stdGen st)
    getSuccs = curSuccs `liftM` get
    putSuccs :: [(Int,Int)] -> ModelState
    putSuccs a = do
      st <- get
      put st { curSuccs = a }


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
  when (len <= 0) $ fail "can't produce empty text"
  let inFile = "sokal.model"
  modelStrs <- lines <$> readFile inFile 
  let modelTpls = read <$> modelStrs
  let upBound = length modelTpls
  let fastModel = array (1,upBound) (zip [1..] modelTpls)
  gen <- getStdGen
  let output = evalState (execWriterT (runModel len fastModel)) $ InnerState gen []
  let cleaned = tail $ dropWhile ((/= '.') . last) output
  putStr $ linefill 72 cleaned
