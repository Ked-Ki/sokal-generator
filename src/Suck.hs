module Suck where

import Network.HTTP
import Text.HTML.TagSoup
import Data.Map

type PrimitiveModel = Map (String,String) [String]
type ProcessedModel = [(String,[(Int,Int)]]


suck :: FilePath -> IO ()
suck urlFile = undefined
