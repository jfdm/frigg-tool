-- -------------------------------------------------------------- [ Schema.idr ]
-- Module    : Schema.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Schema

schemaRNC : String
schemaRNC = """
start = pattern

pattern = element pattern {
   identity,
   element name { score, text },
   element abstract { score, text },
   metadata,
   element context { score, text },
   problem,
   solution,
   element evidence { score, text },
   advice,
   studies,
   relations
}

metadata = element mdata {
   element aliases { element alias { text }},
   element tags  { element tag { text }},
   element created { text },
   element modified { text },
   element evaluated { text},
   element author { text },
   element evaluator { text }
}

requirements = element requirements {
   score,
   element functional { identity, title, description }*,
   element usability { identity, title, description }*,
   element reliability { identity, title, description }*,
   element performance { identity, title, description }*,
   element supportability { identity, title, description }*
}

problem = element problem {
   score,
   element title { text },
   description,
   requirements
}


solution = element solution {
   description,
   element structure {
      score,
      description,
      element model {
         modelTy,
         text
      }
   },
   element dynamics {
      score,
      description,
      element model {
         modelTy,
         text
      }
   },
   element properties {
      score,
      element property {
         title,
         description,
         element action {
            evalValue,
            identity,
            description,
            element actson {
               contribValue,
               attribute forceID { text },
               text
            }+,
            element effects {
               contribValue,
               attribute actionID { text },
               text
            }*
         }+
      }+
   }
}

advice = element advice {
   score,
   element pro { text }*,
   element con { text }*
}

studies = element studies {
   score,
   element study {
      description,
      element fixed { text }
   }+
}

relations = element relations {
   score,
   element link {
      relationship,
      attribute patternID { text },
      text
   }*
}

title = element title { text }

identity = attribute id { text }

description = element desc { text }

score = attribute score {
   "A" | "B" | "C" | "D" | "E"
}

evalValue = attribute evalValue {
   "satisfied" | "weaksatis" | "weakden" | "denied" | "unknown"
}

contribValue = attribute contribValue {
   "makes" | "helps" | "some-positive" | "zero" | "some-negative" | "hurts" | "breaks"
}

relationship = attribute relationship {
   "specialises" | "implements" | "uses" | "linkedTo"
}

modelTy = attribute modelTy {
   "class" | "component" | "sequence" | "deployment"
}
"""
-- --------------------------------------------------------------------- [ EOF ]