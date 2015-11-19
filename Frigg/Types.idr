module Frigg.Types


import Sif.Types
import Sif.Pattern
import Readability
import Readability.Process.XML


data PEvalResult : Type where
  ResSif  : EvalResult -> PEvalResult
  ResRead : ReadResult -> PEvalResult
  ResWTA  : Float      -> PEvalResult

instance Show PEvalResult where
  show (ResSif  res) = show res
  show (ResRead res) = show res
  show (ResWTA  res) = show res

-- --------------------------------------------------------------------- [ EOF ]