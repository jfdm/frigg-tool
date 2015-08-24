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

-- -------------------------------------------------------------------- [ Mode ]

data FriggMode = Eval | Conv | REPL | VERS | HELP | Schema
data FriggOutFormat = FXML | FMDOWN | FORG

readFriggOutFormat : String -> Maybe FriggOutFormat
readFriggOutFormat str =
  case toLower str of
    "xml" => Just FXML
    "md"  => Just FMDOWN
    "org" => Just FORG
    otherwise => Nothing

-- ----------------------------------------------------------------- [ Options ]
record FriggOpts where
  constructor MkFriggOpts
  mode    : Maybe FriggMode
  patt    : Maybe String
  weights : Maybe String
  gscale  : Maybe String
  to      : Maybe FriggOutFormat
  out     : Maybe String
  banner  : Bool

mkDefOpts : FriggOpts
mkDefOpts = MkFriggOpts
  (Just HELP) Nothing Nothing Nothing Nothing Nothing
  False

instance Default FriggOpts where
  default = mkDefOpts

convOpts : Arg -> FriggOpts -> Maybe FriggOpts
convOpts (Files (x::xs)) o = Nothing
convOpts (KeyValue k v)  o =
  case k of
    "pattern" => Just $ record {patt    = Just v} o
    "weights" => Just $ record {weights = Just v} o
    "gscale"  => Just $ record {gscale  = Just v} o
    "to"      => Just $ record {to      = readFriggOutFormat v} o
    otherwise => Nothing
convOpts (Flag x) o =
  case x of
    "help"     => Just $ record {mode    = Just HELP}   o
    "version"  => Just $ record {mode    = Just VERS}   o
    "eval"     => Just $ record {mode    = Just Eval}   o
    "conv"     => Just $ record {mode    = Just Conv}   o
    "nobanner" => Just $ record {banner  = False}       o
    "schema"   => Just $ record {mode    = Just Schema} o
    otherwise => Nothing

friggBanner : String
friggBanner = """
    ______     _                ______            __
   / ____/____(_)___ _____ _   /_  __/___  ____  / /
  / /_  / ___/ / __ `/ __ `/    / / / __ \/ __ \/ /
 / __/ / /  / / /_/ / /_/ /    / / / /_/ / /_/ / /
/_/   /_/  /_/\__, /\__, /    /_/  \____/\____/_/
             /____//____/

http://www.github.com/jfdm/frigg-tool
Type :? for help

Frigg is free software with ABSOLUTELY NO WARRANTY.
"""


friggHelpStr : String
friggHelpStr = """Frigg (C) Jan de Muijnck-Hughes 2015

Available Options:

Flag                 | Description
---------------------|----------------------------------------------------------
--pattern="<fname>"  | The pattern to be analysed.
--weights="<fname>"  | A problem specification.
--gscale="<fname"    | A solution specification.
--to="<fmt>"         | The output format.

--help               | Display help
--version            | Display version
--eval               | Evaluate problem solution pairing
--conv               | Convert to problem solution pairing
--schema             | Show XML Schema
--nobanner           | Don't display banner
"""


-- --------------------------------------------------------------------- [ EOF ]
