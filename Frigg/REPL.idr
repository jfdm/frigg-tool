-- ---------------------------------------------------------------- [ REPL.idr ]
-- Module    : REPL.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.REPL

import XML.DOM
import XML.Reader

import Freyja
import Freyja.Convert
import Freyja.Extract

import Readability

import Frigg.Effs
import Frigg.Options
import Frigg.Error
import Frigg.Eval
import Frigg.API
import Frigg.Commands

-- ------------------------------------------------------------------- [ Begin ]

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

||| Fetch and parse commands
fetchCMD : Frigg FriggCMD
fetchCMD = do
    putStr "frigg> "
    rawCmd <- getStr
    case parseCMD rawCmd of
      Left err => do
        printLn NoSuchCommand
        fetchCMD
      Right cmd => pure cmd


doCommand : FriggCMD -> Frigg ()
doCommand Quit             = pure ()
doCommand Help             = putStrLn showHelp
doCommand (Display part)   = doDisplay part
doCommand (Convert outfmt) = doConvertShow outfmt
doCommand (Query qStr)     = doQuery qStr
doCommand (Eval  what)     = doEvalAndReport what

runREPL : Frigg ()
runREPL = do
    cmd <- fetchCMD
    case cmd of
      Quit => pure ()
      x    => do
        doCommand x
        runREPL

||| A Viewer to view a library of patterns.
export
friggREPL : Frigg ()
friggREPL =
  case banner !(getOptions) of
    True => do
      putStrLn friggBanner
      runREPL
    False => runREPL

-- --------------------------------------------------------------------- [ EOF ]
