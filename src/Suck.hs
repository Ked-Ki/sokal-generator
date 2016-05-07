import Network.Browser (browse, request, setOutHandler)
import Network.HTTP (getRequest)
import Network.HTTP.Base (rspBody)
import Text.HTML.TagSoup
import System.Environment (getArgs)

import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Function (on)
import Data.Ord
import Data.List (sortBy)
import Data.Maybe

type PrimitiveModel = Map.Map (String,String) [String]
type FreqModel = Map.Map (String,String) [(Int,String)]
type ProcessedModel = [(String,[(Int,Int)])]


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

simplifyModel :: PrimitiveModel -> FreqModel
simplifyModel m = Map.map toFreq m
  where
    toFreq l = sortBy (compare `on` (Down . fst)) $ map (\(a,b) -> (b,a)) $ 
               Map.toList $ toFreq' Map.empty l

    toFreq' :: Map.Map String Int -> [String] -> Map.Map String Int
    toFreq' accum [] = accum
    toFreq' accum (w:ws) = toFreq' (Map.insertWith (+) w 1 accum) ws

mkPrcModel :: FreqModel -> ProcessedModel
mkPrcModel m = map processState $ Map.toList m
  where 
    processState ((x,y),freqs) = (y, mapMaybe (mkIdx y) freqs)
    mkIdx y (i,str) = do
      idx <- Map.lookupIndex (y,str) m
      return (i,idx)

process :: [String] -> ProcessedModel
process = mkPrcModel . simplifyModel . mkPrimModel

harvestText :: [Tag String] -> [String]
harvestText tags = concatMap words $ harvest' False tags
  where
    harvest' state tags = case (state,tags) of
    -- state is True inside <p> tags and False outside them
      (False, (TagOpen "p" _): ts) -> harvest' True ts 
      (True,  (TagClose "p") : ts) -> harvest' False ts
      (True,  (TagText str)  : ts) -> str : harvest' True ts
      (_, _:ts) -> harvest' state ts
      (_, []) -> []

main :: IO ()
main = do
  let outFile = "sokal.model"
  writeFile outFile ""
  args <- getArgs
  let urlFile = head args
  contents <- readFile urlFile
  rawWords <- forM (take 1 (words contents)) $ \url -> do
    (_,resp) <- browse $ do
       setOutHandler $ const (return ()) -- silence logging of HTTP headers
       request $ getRequest url
    return $ (harvestText . parseTags . rspBody) resp
  let allWords = concat rawWords
  let prcModel = process allWords
  mapM_ (appendFile outFile . (++ "\n") . show) prcModel 
