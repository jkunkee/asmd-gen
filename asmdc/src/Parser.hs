module Parser where

import ASMD
import qualified ASMD.Sectioning as Sections

asmd = parseASMD . Sections.parse
