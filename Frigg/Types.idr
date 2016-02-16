-- --------------------------------------------------------------- [ Types.idr ]
-- Module    : Types.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Types

import Sif.Types
import Sif.Pattern
import Readability
import Readability.Process.XML

public export
data PEvalResult : Type where
  ResSif  : EvalResult -> PEvalResult
  ResRead : ReadResult -> PEvalResult
  ResWTA  : Float      -> PEvalResult

Show PEvalResult where
  show (ResSif  res) = show res
  show (ResRead res) = show res
  show (ResWTA  res) = show res

-- --------------------------------------------------------------------- [ EOF ]
