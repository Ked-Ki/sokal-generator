import Data.Array

type FastModel = Array Int (String,[(Int,Int)])

main :: IO ()
main = do
  let inFile = "sokal.model"
  modelStrs <- lines <$> readFile inFile 
  let upBound = length modelStrs
  let fastModel = array (1,upBound) (zip [1..] modelStrs)
  putStrLn "still stub"
