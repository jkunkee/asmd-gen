{-# LANGUAGE OverloadedStrings #-}
module ASMD.Section where

import Data.Text (Text)

data SectionKind = Options
                 | GenericParameters
                 | Inputs
                 | Outputs
                 | Registers
                 | States
                 | Conditions
                 | Invalid
                 deriving (Show)

data Section = Section { kind :: SectionKind
                       , body :: [Text]
                       } deriving (Show)

emptySection :: SectionKind -> Section
emptySection k = Section { kind = k, body = [] }
