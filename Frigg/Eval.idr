-- ---------------------------------------------------------------- [ Eval.idr ]
-- Module    : Eval.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Eval

import Data.AVL.Dict

import Readability
import Readability.Process.XML

import Config.INI

import XML.DOM
import XML.XPath
import XML.XPath.Query

import Freyja.Utils

import Frigg.Options
import Frigg.Error
import Frigg.Effs
import Frigg.Config

-- -------------------------------------------------------------- [ Directives ]

%access private

-- ------------------------------------------------------------------- [ Begin ]


-- @TODO Make nicers
getScore : (heading  : String)
        -> (gradings : Dict String Double)
        -> (doc      : XMLDoc)
        -> Either FriggError Double
getScore t gsc doc = do
    case query (concat ["//", toLower t, "/@score"]) doc of
      Left err => Left $ ExtractionError err
      Right xs => case getText xs of
        Nil     => Left $ EvalError $ unwords ["Node Not Found for:", show t]
        (x::xs) => case getNodeValue x of
          Nothing => Left $ EvalError $ unwords ["Score Not Given for:", show t]
          Just v => case Dict.lookup (toLower v) gsc of
            Nothing => Left $ EvalError $ unwords ["Weighting not found for:", show v]
            Just s  => Right s

calcTemplateAdherence : Dict String Double
                     -> Dict String Double
                     -> Document DOCUMENT
                     -> Either FriggError Double
calcTemplateAdherence ws gsc doc = do
    ss <- mapEither (\(k,v) => doCalc k v) $ Dict.toList ws
    pure $ foldl (+) 0.0 ss
  where
    doCalc : String -> Double -> Either FriggError Double
    doCalc t weight = do
        res <- getScore t gsc doc
        pure (weight * res)

export
evalTemplate : Frigg (Either FriggError Double)
evalTemplate = do
    doc <- getXMLDoc
    scale <- getGradingScale
    weight <- getTemplateWeightings
    case (isEmpty scale, isEmpty weight) of
        (False, False) => pure $ calcTemplateAdherence weight scale doc
        otherwise      => pure $ Left InvalidMapping

export
evalTemplateAndReport : Frigg ()
evalTemplateAndReport = do
  (Right res) <- evalTemplate | Left err => printLn err
  printLn res

-- --------------------------------------------------------------------- [ EOF ]
