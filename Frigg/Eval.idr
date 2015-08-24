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

import Frigg.Options
import Frigg.Error
import Frigg.Effs
import Frigg.RCard

-- -------------------------------------------------------------- [ Directives ]

%access private

public
record EvalResult where
  constructor MkEvalResult
  readability : ReadResult
  tAdherence  : Float

instance Show EvalResult where
  show (MkEvalResult x y) = unlines
      [ "Automated Eval Result:"
      , unwords ["\tReadability Score:", show x]
      , unwords ["\tTemplate Adherence", show y]
      ]

getMappings : INIElem -> Dict String Float
getMappings (INIFile is) =
    Dict.fromList $ catMaybes $ map getMap is
  where
    getMap : INIElem -> Maybe (String, Float)
    getMap (INIFile is)       = Nothing
    getMap (INISection t kvs) = Nothing
    getMap (INIEntry k v)     =
      let val = (the (Float) (cast v)) in
        case val < 0 of
          True  => Nothing
          False => Just (toLower k, val)

getConfig : Maybe String -> Eff (Dict String Float) FriggEffs
getConfig Nothing   = pure empty
getConfig (Just fn) =
  case !(readINIConfig fn) of
    Left  err => pure empty
    Right res => pure $ getMappings res

getTemplateWeightings : Eff (Dict String Float) FriggEffs
getTemplateWeightings = getConfig $ weights !getOptions

getGradingScale : Eff (Dict String Float) FriggEffs
getGradingScale = getConfig $ gscale !getOptions

calcTemplateAdherence : Dict String Float
                     -> Dict String Float
                     -> Document DOCUMENT
                     -> Float
calcTemplateAdherence ws gsc doc =
    Dict.foldr (\x,y,z => z + doCalc x y) 0.0 ws
  where
    score : String -> Float
    score k' =
      case queryDoc (toLower k' ++ "/@score") doc of
        Left err => 0.0
        Right xs => case getText xs of
          Nil     => 0.0
          (x::xs) => case getNodeValue x of
            Nothing => 0.0
            Just v => case Dict.lookup (toLower v) gsc of
              Just s  => s
              Nothing => 0.0

    doCalc : String -> Float -> Float
    doCalc k weight = weight * (score k)

public
eval : Document DOCUMENT -> Eff (Either FriggError EvalResult) FriggEffs
eval doc = do
  case calcReadability doc of
    Nothing      => pure $ Left MalformedPattern
    Just readRes => do
      scale <- getGradingScale
      weight <- getTemplateWeightings
      case (isEmpty scale, isEmpty weight) of
        (False, False) => do
          let tadd = calcTemplateAdherence weight scale doc
          pure $ Right (MkEvalResult readRes tadd)
        otherwise => pure $ Left InvalidMapping

-- --------------------------------------------------------------------- [ EOF ]
