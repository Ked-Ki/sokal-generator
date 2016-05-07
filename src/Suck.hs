--IO Libraries
import Network.Browser (browse, request, setOutHandler)
import Network.HTTP (getRequest)
import Network.HTTP.Base (rspBody)
import System.Environment (getArgs)

import qualified Text.HTML.TagSoup as TS
import qualified Data.Map as Map

import Control.Monad (liftM)
import Data.Char (isAscii)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Data.Ord

-- Building & Processing the Model

type PrimitiveModel = Map.Map (String,String) [String]

mkPrimModel :: [String] -> PrimitiveModel
mkPrimModel words = mkPrimModel' ("","") words Map.empty
  where
    mkPrimModel' :: (String,String) -> 
                    [String] -> 
                    Map.Map (String,String) [String] -> 
                    Map.Map (String,String) [String]
    mkPrimModel' state (w:ws) map = mkPrimModel' (snd state, w) ws $ 
                                      Map.insertWith (++) state [w] map
    mkPrimModel' state [] map = map


type ProcessedModel = [(String,[(Int,Int)])]

mkPrcModel :: PrimitiveModel -> ProcessedModel
mkPrcModel m = map processState $ Map.toList $ Map.map toFreq m
  where 
    toFreq l = sortBy (compare `on` (Down . fst)) $ toFreq' [] l

    toFreq' :: [(Int,String)] -> [String] -> [(Int,String)]
    toFreq' accum [] = accum
    toFreq' accum (w:ws) = toFreq' ((cnt,w):accum) ws'
      where
        ws' = filter (/= w) ws
        cnt = (length ws) - (length ws') + 1

    processState ((x,y),freqs) = (y, mapMaybe (mkIdx y) freqs)
    mkIdx y (i,str) = do
      idx <- Map.lookupIndex (y,str) m
      return (i,idx)

-- Fetching and cleaning the text from URLs
--
mkModelFromUrl :: String -> IO PrimitiveModel
mkModelFromUrl url = mkPrimModel <$> fetchWords url

fetchWords :: String -> IO [String]
fetchWords url = do
  putStrLn $ "fetching " ++ url
  (_,resp) <- browse $ do
     setOutHandler $ const (return ()) -- silence logging of HTTP headers
     request $ getRequest url
  return $ (harvestText . TS.parseTags . rspBody) resp

harvestText :: [TS.Tag String] -> [String]
harvestText tags = concatMap words $ harvest' False tags
  where
    harvest' state tags = case (state,tags) of
    -- state is True inside <p> tags and False outside them
      (False, (TS.TagOpen "p" _): ts) -> harvest' True ts 
      (True,  (TS.TagClose "p") : ts) -> harvest' False ts
      (True,  (TS.TagText str)  : ts) -> clean str : harvest' True ts
      (_, _:ts) -> harvest' state ts
      (_, []) -> []
    clean = filter isAscii

outputFilename :: FilePath
outputFilename = "sokal.model"

main :: IO ()
main = do
  writeFile outputFilename ""
  args <- getArgs
  let urlFile = head args
  contents <- readFile urlFile
  primModels <- mapM (mkModelFromUrl) $ words contents
  putStrLn "combining models..."
  let combModel = Map.unionsWith (++) primModels
  putStrLn "processing model..."
  let prcModel = mkPrcModel combModel
  mapM_ (appendFile outputFilename . (++ "\n") . show) prcModel 
