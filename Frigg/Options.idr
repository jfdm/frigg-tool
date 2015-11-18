-- ------------------------------------------------------------- [ Options.idr ]
-- Module    : Options.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Options

-- ----------------------------------------------------------------- [ Imports ]
import Effects
import Effect.Exception

import ArgParse

import XML.DOM
import XML.Reader

import Freyja.Convert

-- -------------------------------------------------------------------- [ Mode ]

data FriggMode = Eval | Conv | REPL | VERS | HELP | Sif

-- ----------------------------------------------------------------- [ Options ]
record FriggOpts where
  constructor MkFriggOpts
  mode    : Maybe FriggMode
  patt    : Maybe String
  weights : Maybe String
  gscale  : Maybe String
  to      : Maybe FreyjaOutFormat
  out     : Maybe String
  banner  : Bool

mkDefOpts : FriggOpts
mkDefOpts = MkFriggOpts
  (Just HELP) Nothing Nothing Nothing Nothing Nothing
  True

instance Default FriggOpts where
  default = mkDefOpts

convOpts : Arg -> FriggOpts -> Maybe FriggOpts
convOpts (Files (x::xs)) o = Nothing
convOpts (KeyValue k v)  o =
  case k of
    "pattern" => Just $ record {patt    = Just v} o
    "weights" => Just $ record {weights = Just v} o
    "gscale"  => Just $ record {gscale  = Just v} o
    "to"      => Just $ record {to      = readOutFMT v} o
    otherwise => Nothing
convOpts (Flag x) o =
  case x of
    "sif"      => Just $ record {mode    = Just Sif}    o
    "help"     => Just $ record {mode    = Just HELP}   o
    "version"  => Just $ record {mode    = Just VERS}   o
    "eval"     => Just $ record {mode    = Just Eval}   o
    "conv"     => Just $ record {mode    = Just Conv}   o
    "repl"     => Just $ record {mode    = Just REPL}   o
    "nobanner" => Just $ record {banner  = False}       o
    otherwise => Nothing

friggHelpStr : String
friggHelpStr = """Frigg (C) Jan de Muijnck-Hughes 2015

Available Options:

Flag                 | Description
---------------------|----------------------------------------------------------
--pattern="<fname>"  | The pattern to be analysed.
--weights="<fname>"  | A problem specification.
--gscale="<fname"    | A solution specification.
--to="<fmt>"         | The output format.
--sif                | Evaluate embedded sif model.

--help               | Display help
--version            | Display version
--eval               | Evaluate problem solution pairing
--conv               | Convert to problem solution pairing
--repl               | REPL
--nobanner           | Don't display banner
"""


-- --------------------------------------------------------------------- [ EOF ]
