import Network.Browser (browse, request, setOutHandler)
import Network.HTTP (getRequest)
import Network.HTTP.Base (rspBody)
import Text.HTML.TagSoup
import qualified Data.Map as Map
import System.Environment (getArgs)

import Control.Monad (forM_)
import Control.Monad.State

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
simplifyModel m = undefined

processModel :: FreqModel -> ProcessedModel
processModel m = undefined

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
  let prcModel = (processModel . simplifyModel . mkPrimModel) allWords
  mapM_ (appendFile outFile . (++ "\n") . show) prcModel 
