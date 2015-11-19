-- -------------------------------------------------------------- [ Config.idr ]
-- Module    : Config.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Config

import Data.AVL.Dict

import Readability
import Readability.Process.XML

import Config.INI

import XML.DOM
import XML.XPath

import Frigg.Options
import Frigg.Error
import Frigg.Effs

-- -------------------------------------------------------------- [ Directives ]

%default partial
%access public

-- ------------------------------------------------------------------- [ Begin ]

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

getConfig : Maybe String -> Frigg (Dict String Float)
getConfig Nothing   = pure empty
getConfig (Just fn) =
  case !(readINIConfig fn) of
    Left  err => pure empty
    Right res => pure $ getMappings res

getTemplateWeightings : Frigg (Dict String Float)
getTemplateWeightings = getConfig $ weights !getOptions

getGradingScale : Eff (Dict String Float) FriggEffs
getGradingScale = getConfig $ gscale !getOptions


-- --------------------------------------------------------------------- [ EOF ]
