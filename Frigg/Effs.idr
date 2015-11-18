-- --------------------------------------------------------- [ Effs.idr<Frigg> ]
-- Module    : Effs.idr<Frigg>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Effs

-- ----------------------------------------------------------------- [ Imports ]
import public Effects
import public Effect.System
import public Effect.State
import public Effect.Exception
import public Effect.File
import public Effect.StdIO

import XML.DOM

import ArgParse

import Config.INI

import Freyja

import Frigg.Options
import Frigg.Error

-- -------------------------------------------------------------- [ Directives ]

%access public

-- ------------------------------------------------------------------- [ State ]

record FriggState where
  constructor MkFriggState
  opts : FriggOpts
  pdoc : Maybe PatternDoc
  xdoc : Maybe XMLDoc

instance Default FriggState where
  default = MkFriggState mkDefOpts Nothing Nothing

-- ----------------------------------------------------------------- [ Effects ]

FriggEffs : List EFFECT
FriggEffs = [ FILE_IO ()
            , SYSTEM
            , STDIO
            , 'ferr     ::: EXCEPTION FriggError
            , 'fstate   ::: STATE FriggState
          ]

namespace Frigg
  raise : FriggError -> Eff b ['ferr ::: EXCEPTION FriggError]
  raise err = 'ferr :- Exception.raise err

Frigg : Type -> Type
Frigg rTy = Eff rTy FriggEffs

getOptions : Eff FriggOpts ['fstate ::: STATE FriggState]
getOptions = pure $ opts !('fstate :- get)

putOptions : FriggOpts -> Eff () ['fstate ::: STATE FriggState]
putOptions o = 'fstate :- update (\st => record {opts = o} st)

processOptions : Frigg FriggOpts
processOptions =
    case parseArgs mkDefOpts convOpts !getArgs of
      Left err => Frigg.raise (ParseError (show err))
      Right  o => pure o

putPatternDoc : PatternDoc -> Eff () ['fstate ::: STATE FriggState]
putPatternDoc p = 'fstate :- update (\st => record {pdoc = Just p} st)

getPatternDoc : Frigg PatternDoc
getPatternDoc =
    case pdoc !('fstate :- get) of
      Nothing => Frigg.raise PatternDocMissing
      Just p  => pure p

getXMLDoc : Frigg XMLDoc
getXMLDoc =
    case xdoc !('fstate :- get) of
      Nothing => Frigg.raise PatternDocMissing
      Just x  => pure x

putXMLDoc : XMLDoc -> Eff () ['fstate ::: STATE FriggState]
putXMLDoc p = 'fstate :- update (\st => record {xdoc = Just p} st)

-- --------------------------------------------------------------------- [ EOF ]
