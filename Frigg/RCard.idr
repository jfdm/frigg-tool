-- --------------------------------------------------------------- [ RCard.idr ]
-- Module    : RCard.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.RCard

data LetterScale = A | B | C | D | E
data GBUScale = Good | Bad | Ugly

data Grade : Type -> Type where
  MkGrade : (value : a) -> (remark : String) -> Grade a

value : Grade a -> a
value (MkGrade val _) = val

remark : Grade a -> String
remark (MkGrade _ rmk) = rmk

record RCard where
  constructor MkRCard
  coherency   : Maybe (Grade LetterScale)
  atomicity   : Maybe (Grade LetterScale)
  problemIndy : Maybe (Grade LetterScale)
  soltApprops : Maybe (Grade Float)
  soltComplex : Maybe (Grade GBUScale)
  soltEff     : Maybe (Grade LetterScale)
  pattStruct  : Maybe (Grade Float)
  presLeg     : Maybe (Grade Float)
  presAccess  : Maybe (Grade LetterScale)

-- --------------------------------------------------------------------- [ EOF ]
