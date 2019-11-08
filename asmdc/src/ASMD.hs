{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD where

import ASMD.Section
import ASMD.Parseable
import ASMD.Options
import ASMD.GenericsParameters
import ASMD.Inputs
import ASMD.Outputs
import ASMD.Registers
import ASMD.States
import ASMD.Conditions

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data ASMD = ASMD { options            :: [Option]
                 , genericsParameters :: [GenericParameter]
                 , inputs             :: [Input]
                 , outputs            :: [Output]
                 , registers          :: [Register]
                 , states             :: [State]
                 , conditions         :: [Condition]
                 } deriving (Generic,Show)

instance ToJSON ASMD
instance FromJSON ASMD

emptyAsmd = (ASMD [] [] [] [] [] [] [])

parseASMD :: [Section] -> ASMD
parseASMD = L.foldl parse' emptyAsmd

parseInto :: (Parseable s) => [Text] -> [s] -> [s]
parseInto src dest = (parse src) ++ dest

parse' :: ASMD -> Section -> ASMD
parse' asmd@(ASMD opt g i o r s c) (Section k b) =
  case k of 
    Options           -> (ASMD (update opt) g i o r s c)
    GenericParameters -> (ASMD opt (update g) i o r s c)
    Inputs            -> (ASMD opt g (update i) o r s c)
    Outputs           -> (ASMD opt g i (update o) r s c)
    Registers         -> (ASMD opt g i o (update r) s c) 
    States            -> (ASMD opt g i o r (update s) c)
    Conditions        -> (ASMD opt g i o r s (update c))
    Invalid           -> asmd
  where 
    update :: (Parseable s) => [s] -> [s]
    update = parseInto b
