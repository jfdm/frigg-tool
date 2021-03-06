-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Error

import XML.XPath

%access export

public export
data FriggError : Type where
  FeatureNotImpl    : FriggError
  NoFormatSpecified : FriggError
  NoModeSpecified   : FriggError
  FileExpected      : FriggError
  MissingFile       : String -> FriggError
  MalformedPattern  : FriggError
  InvalidMapping    : FriggError
  PatternDocMissing : FriggError
  NoSuchCommand     : FriggError
  ParseError        : String -> FriggError
  GeneralError      : String -> FriggError
  EvalError         : String -> FriggError
  ExtractionError   : XPathError -> FriggError

Show FriggError where
  show NoSuchCommand     = "Command Not Recognised"
  show PatternDocMissing = "External Document needs to be specified."
  show NoModeSpecified   = "No Mode Specified"
  show NoFormatSpecified = "No output format specified"
  show FeatureNotImpl    = "Feature Not Implemented"
  show MalformedPattern  = "Pattern is malformed."
  show InvalidMapping    = "Mappings must be greater than zero"
  show FileExpected      = "File Expected"

  show (GeneralError msg) = unwords ["General Error", msg]
  show (MissingFile fn)   = unwords ["Missing file:", fn]
  show (ParseError  msg)  = unlines ["Parse Error:", msg]
  show (EvalError fn)     = unwords ["Error During Evaluation:", fn]
  show (ExtractionError msg) = unwords ["Extraction Error", show msg]

-- --------------------------------------------------------------------- [ EOF ]
