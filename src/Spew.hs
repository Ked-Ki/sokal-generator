import Data.Array
import Control.Monad.State
import System.Random
import System.Environment (getArgs)

type FastModel = Array Int (String,[(Int,Int)])

runModel :: Int -> FastModel -> State StdGen String
runModel len model = do
  startIx <- state $ randomR (1,snd $ bounds model)   
  let startSt = model ! startIx
  fst <$> foldM genOneWord startSt [2..len] 
  where
    genOneWord (str,next) _ = do
      succIx <- state $ randomR (0,length next - 1)
      let newIx = snd $ next !! succIx
      let (newStr, newNext) = model ! newIx
      return (str ++ " " ++ newStr, newNext)

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
  putStrLn output
