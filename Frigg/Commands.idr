-- ------------------------------------------------------------ [ Commands.idr ]
-- Module    : Commands.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Commands

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Freyja.Convert

word : Parser String
word = lexeme (map pack $ some (alphaNum) ) <?> "Identity"

-- ---------------------------------------------------------------- [ Commands ]

public export
data DisplayPattern = All
                    | Summary
                    | MData
                    | Context
                    | Problem
                    | Solution
                    | Evidence
                    | Studies
                    | Related

public export
data EvalPattern : Type where
  EvalAll     : EvalPattern
  Readability : EvalPattern
  TemplateAd  : EvalPattern
--  SolutionApp : EvalPattern
  SifModel    : EvalPattern

public export
data FriggCMD : Type where
  Query   : String -> FriggCMD
  Display : DisplayPattern  -> FriggCMD
  Convert : FreyjaOutFormat -> FriggCMD
  Eval    : EvalPattern     -> FriggCMD
  Quit : FriggCMD
  Help : FriggCMD

export
showHelp : String
showHelp = """
Command                 | Description
------------------------|-------------------------------------------------------
:display <what>         | Display part of the pattern. One Of:
                        | [ all, info, metadata, context, problem,
                        |   solution, evidence, studies, related]
                        |
:evaluate <what>        | Perform an evaluation. One of:
                        | [sif, read, template]
                        |
:convert <to>           | Convert to a valid out format: One of:
                        | [latex, org, markdown, xml ]
                        |
:query <string>         | Query XML Doc using XPath
                        |
:quit :q :exit          | Quit the repl
:? :help                | Show this help
"""


display : Parser FriggCMD
display = do
      string ":display"
      space
      o <- displayOpts
      pure $ Display o
    <?> "Display"
  where
    displayOpts : Parser (DisplayPattern)
    displayOpts = (string "all"      *> return All)
              <|> (string "info"     *> return Summary)
              <|> (string "metadata" *> return MData)
              <|> (string "context"  *> return Context)
              <|> (string "problem"  *> return Problem)
              <|> (string "solution" *> return Solution)
              <|> (string "evidence" *> return Evidence)
              <|> (string "studies"  *> return Studies)
              <|> (string "related"  *> return Related)


convert : Parser FriggCMD
convert = do
    string ":convert"
    space
    fmt <- format
    pure $ Convert fmt
  where
    format : Parser (FreyjaOutFormat)
    format = do
      w <- word
      case (readOutFMT w) of
        Just fmt => pure fmt
        Nothing  => fail "Expected out format"

evaluate : Parser FriggCMD
evaluate = do
    string ":evaluate"
    space
    evl <- aspect
    pure $ Eval evl
  where
    aspect : Parser EvalPattern
    aspect = (string "sif"      *> return SifModel)
         <|> (string "read"     *> return Readability)
         <|> (string "template" *> return TemplateAd)
         <|> (string "all"      *> return EvalAll)

query : Parser FriggCMD
query = do
    string ":query"
    space
    qStr <- quoted '"'
    pure $ Query qStr

quit : Parser FriggCMD
quit = (string ":quit" *> return Quit)
   <|> (string ":q"    *> return Quit)
   <|> (string ":exit" *> return Quit)

help : Parser FriggCMD
help = (string ":?"    *> return Help)
   <|> (string ":help" *> return Help)

cmd : Parser FriggCMD
cmd = convert
  <|> evaluate
  <|> display
  <|> query
  <|> quit
  <|> help
  <?> "Command"

export
parseCMD : String -> Either String FriggCMD
parseCMD s = parse cmd s

-- --------------------------------------------------------------------- [ EOF ]
