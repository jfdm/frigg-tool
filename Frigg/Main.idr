-- ---------------------------------------------------------------- [ Main.idr ]
-- Module    : Main.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Main

import System

import XML.DOM
import XML.Reader

import Freyja
import Freyja.Extract

import Readability

import Frigg.Effs
import Frigg.Options
import Frigg.Error
import Frigg.Eval
import Frigg.API
import Frigg.REPL

execMode : Maybe FriggMode -> Frigg ()
execMode Nothing       = printLn NoModeSpecified
execMode (Just VERS)   = printLn FeatureNotImpl
execMode (Just HELP)   = putStrLn friggHelpStr

execMode (Just REPL)  = friggREPL
execMode (Just Conv)  = convertShowDoc
execMode (Just Read)  = evalReadAndReport
execMode (Just Sif)   = evalSifAndReport
execMode (Just Templ) = evalTemplateAndReport

friggMain : Eff () FriggEffs
friggMain = do
  o <- processOptions
  putOptions o
  loadPatternDoc (patt o)
  execMode (mode o)

main : IO ()
main = do
  run $ friggMain
  exit 0

-- --------------------------------------------------------------------- [ EOF ]
