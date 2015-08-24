-- ---------------------------------------------------------------- [ Main.idr ]
-- Module    : Main.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Main

import System

import XML.DOM
import XML.Reader

import Readability

import Frigg.Effs
import Frigg.Options
import Frigg.Convert
import Frigg.Error
import Frigg.Schema
import Frigg.Eval

printLnEither : (Show a, Show b) => Either a b -> Eff () [STDIO]
printLnEither (Left  x) = printLn x
printLnEither (Right x) = printLn x

execMode : Maybe FriggMode -> Eff () FriggEffs
execMode Nothing       = printLn NoModeSpecified
execMode (Just VERS)   = printLn FeatureNotImpl
execMode (Just HELP)   = putStrLn friggHelpStr
execMode (Just Schema) = putStrLn schemaRNC

execMode (Just REPL) = printLn FeatureNotImpl
execMode (Just Conv) = printLn FeatureNotImpl
execMode (Just Eval) = do
    case patt !getOptions of
      Nothing => Frigg.raise FileExpected
      Just fn => do
        case !(readXMLDoc fn) of
          Left err  => printLn err
          Right doc => printLnEither $ !(eval doc)

friggMain : Eff () FriggEffs
friggMain = do
  o <- processOptions
  execMode (mode o)

main : IO ()
main = do
  run $ friggMain
  exit 0
