-- -------------------------------------------------------- [ Error.idr<Frigg> ]
-- Module    : Error.idr<Frigg>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Error

data FriggError : Type where
  FeatureNotImpl    : FriggError
  NoFormatSpecified : FriggError
  NoModeSpecified   : FriggError
  FileExpected      : FriggError
  MissingFile       : String -> FriggError
  MalformedPattern  : FriggError
  InvalidMapping    : FriggError

instance Show FriggError where
  show NoModeSpecified   = "No Mode Specified"
  show NoFormatSpecified = "No output format specified"
  show FeatureNotImpl    = "Feature Not Implemented"
  show MalformedPattern  = "Pattern is malformed."
  show InvalidMapping    = "Mappings must be greater than zero"
  show FileExpected      = "File Expected"

  show (MissingFile fn)  = unwords ["Missing file:",fn]

-- --------------------------------------------------------------------- [ EOF ]
