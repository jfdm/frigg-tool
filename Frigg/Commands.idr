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

%access public

word : Parser String
word = lexeme (map pack $ some (alphaNum) ) <?> "Identity"

-- ---------------------------------------------------------------- [ Commands ]

data DisplayPattern = All
                    | Summary
                    | MData
                    | Context
                    | Problem
                    | Solution
                    | Evidence
                    | Studies
                    | Related

data EvalPattern : Type where
  Readability : EvalPattern
  TemplateAd  : EvalPattern
--  SolutionApp : EvalPattern
  SifModel    : EvalPattern

public
data FriggCMD : Type where
  Display : DisplayPattern  -> FriggCMD
  Convert : FreyjaOutFormat -> FriggCMD
  Eval    : EvalPattern     -> FriggCMD
  Quit : FriggCMD
  Help : FriggCMD

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
:quit :q :exit          | Quit the repl
:? :help                | Show this help
"""

private
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


private
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

private
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

private
quit : Parser FriggCMD
quit = (string ":q"    *> return Quit)
   <|> (string ":quit" *> return Quit)
   <|> (string ":exit" *> return Quit)

private
help : Parser FriggCMD
help = (string ":?"    *> return Help)
   <|> (string ":help" *> return Help)

cmd : Parser FriggCMD
cmd = quit
  <|> convert
  <|> evaluate
  <|> display
  <|> help
  <?> "Command"

public
parseCMD : String -> Either String FriggCMD
parseCMD s = parse cmd s

-- --------------------------------------------------------------------- [ EOF ]
