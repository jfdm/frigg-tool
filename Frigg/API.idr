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
import Readability.Process.XML

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

-- --------------------------------------------------------------- [ Transform ]

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

-- ---------------------------------------------------------------- [ Sif Eval ]
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

-- ----------------------------------------------------------------- [ Display ]

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

-- ------------------------------------------------------------- [ Readability ]

evalReadability : Frigg ReadResult
evalReadability = do
  xdoc <- getXMLDoc
  case calcReadability xdoc of
    Nothing      => Frigg.raise MalformedPattern
    Just readRes => pure readRes

evalReadAndReport : Frigg ()
evalReadAndReport = do
  res <- evalReadability
  putStrLn $ unlines $ map show (Metrics.toList res)


-- ------------------------------------------------------------------- [ Query ]

doQuery : String -> Frigg ()
doQuery qStr = do
  xdoc <- getXMLDoc
  case query qStr xdoc of
    Left err  => printLn err
    Right Nil => putStrLn "Nothing Found"
    Right rs  => putStrLn $ unlines $ map (\r => show @{xml} r) rs

-- ---------------------------------------------------------------- [ Template ]

doEvalAndReport : EvalPattern -> Frigg ()
doEvalAndReport SifModel    = evalSifAndReport
doEvalAndReport TemplateAd  = evalTemplateAndReport
doEvalAndReport Readability = evalReadAndReport
doEvalAndReport EvalAll     = printLn FeatureNotImpl



-- --------------------------------------------------------------------- [ EOF ]
