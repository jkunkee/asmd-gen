{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.Options where

import ASMD.Parseable

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data Option = Option { key   :: Text 
                     , value :: Text 
                     } deriving (Generic,Show)

instance ToJSON Option
instance FromJSON Option

instance Parseable Option where
  parse = L.map (\l -> let (k,v) = T.breakOn "=" l in (Option k $ T.tail v))
