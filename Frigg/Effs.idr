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

import public ArgParse

import Config.INI

import Frigg.Options
import Frigg.Error

-- -------------------------------------------------------------- [ Directives ]

%access public

-- ------------------------------------------------------------------- [ State ]

record FriggState where
  constructor MkFriggState
  opts    : FriggOpts

instance Default FriggState where
  default = MkFriggState mkDefOpts

-- ----------------------------------------------------------------- [ Effects ]

FriggEffs : List EFFECT
FriggEffs = [ FILE_IO ()
            , SYSTEM
            , STDIO
            , 'argparse ::: EXCEPTION ArgParseError
            , 'ferr     ::: EXCEPTION FriggError
            , 'fstate   ::: STATE FriggState
          ]

namespace Frigg
  raise : FriggError -> Eff b ['ferr ::: EXCEPTION FriggError]
  raise err = 'ferr :- Exception.raise err

getOptions : Eff FriggOpts ['fstate ::: STATE FriggState]
getOptions = pure $ opts !('fstate :- get)

putOptions : FriggOpts -> Eff () ['fstate ::: STATE FriggState]
putOptions o = 'fstate :- update (\st => record {opts = o} st)

processOptions : Eff FriggOpts FriggEffs
processOptions = do
    o <- parseArgs mkDefOpts convOpts !getArgs
    putOptions o
    pure o
-- --------------------------------------------------------------------- [ EOF ]
