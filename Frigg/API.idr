-- ----------------------------------------------------------------- [ API.idr ]
-- Module    : API.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.API

import XML.DOM
import XML.Reader
import XML.XPath

import Freyja
import Freyja.Extract
import Freyja.Convert
import Freyja.Convert.Edda

import Edda
import Edda.Writer.Org

import Sif.Types
import Sif.Pattern
import Sif.Pattern.Recover
import Sif.Builder.AbsInterp

import Readability

import Frigg.Effs
import Frigg.Options
import Frigg.Error
import Frigg.Eval
import Frigg.Commands



loadPatternDoc : Maybe String -> Frigg ()
loadPatternDoc Nothing   = Frigg.raise FileExpected
loadPatternDoc (Just fn) = do
  doc <- readXMLDoc fn
  case doc of
    Left err  => printLn $ Frigg.Error.ParseError (show err)
    Right res => do
      putXMLDoc res
      case document res of
        Left err  => printLn $ Frigg.Error.ParseError (show err)
        Right doc => putPatternDoc doc

doConvertShow : FreyjaOutFormat -> Frigg ()
doConvertShow fmt = do
    doc <- getPatternDoc
    case convShow fmt doc of
      Nothing  => Frigg.raise $ GeneralError "Error"
      Just str => putStrLn str

convertShowDoc : Frigg ()
convertShowDoc = do
    case to !getOptions of
      Nothing  => Frigg.raise NoFormatSpecified
      Just fmt => doConvertShow fmt

evalSifRepr : Frigg Sif.EvalResult
evalSifRepr = do
    doc <- getPatternDoc
    let (_ ** sifrep) = fromFreya absInterpBuilder doc
    let res = evalPattern sifrep
    pure res

evalSifAndReport : Frigg ()
evalSifAndReport = do
  res <- evalSifRepr
  case evalResultsToString res of
    Just res => putStrLn res
    Nothing  => putStrLn "oops"

doDisplay : DisplayPattern -> Frigg ()
doDisplay All       = putStrLn (org    (convertPattern !getPatternDoc))
doDisplay MData     = putStrLn (blocks (convertMetadata (mdata    !getPatternDoc)))
doDisplay Context   = putStrLn (blocks (convertDomain   (context  !getPatternDoc)))
doDisplay Problem   = putStrLn (blocks (convertProblem  (problem  !getPatternDoc)))
doDisplay Solution  = putStrLn (blocks (convertSolution (solution !getPatternDoc)))
doDisplay Evidence  = putStrLn (blocks (convertEvidence (evidence !getPatternDoc)))
doDisplay Studies   = putStrLn (blocks (convertStudies  (studies  !getPatternDoc)))
doDisplay Related   = printLn FeatureNotImpl -- @TODO
doDisplay Summary = do
    doc <- getPatternDoc
    putStrLn $ inlines (name doc)
    putStrLn $ blocks  (summary doc)

-- --------------------------------------------------------------------- [ EOF ]
