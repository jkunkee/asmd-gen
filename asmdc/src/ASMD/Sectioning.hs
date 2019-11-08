{-# LANGUAGE OverloadedStrings #-}
module ASMD.Sectioning where

import ASMD.Section
import Data.Text as T
import qualified Data.List as L

parse :: Text -> [Section]
parse = L.reverse . (L.foldl sections []) . T.lines

sections :: [Section] -> Text -> [Section]
sections ast "options:"            = (emptySection Options):ast
sections ast "genericsparameters:" = (emptySection GenericParameters):ast
sections ast "inputs:"             = (emptySection Inputs):ast
sections ast "outputs:"            = (emptySection Outputs):ast
sections ast "registers:"          = (emptySection Registers):ast
sections ast "states:"             = (emptySection States):ast
sections ast "conditions:"         = (emptySection Conditions):ast
sections ast line
  | line == "" || (T.head line) == '#' = ast 
  | L.null ast =  (emptySection Invalid):ast --- If no section occurs before body it's invalid
  | otherwise  = (h { body = (line:(body h)) }):t where (h:t) = ast
