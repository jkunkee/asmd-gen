{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.Registers where

import ASMD.Parseable

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data Register = Register 
  deriving (Generic,Show)

instance ToJSON Register
instance FromJSON Register

instance Parseable Register where
  parse xs = []
