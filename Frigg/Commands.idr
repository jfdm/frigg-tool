-- ------------------------------------------------------------ [ Commands.idr ]
-- Module    : Commands.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Frigg.Commands

import Lightyear
import Lightyear.Strings

%access public

literallyBetweenLR : Char -> Char -> Parser String
literallyBetweenLR l r =
    map pack $ between (lexeme $ char l) (lexeme $ char r) (some (satisfy (/= r)))

literallyBetween : Char -> Parser String
literallyBetween c = literallyBetweenLR c c

word : Parser String
word = lexeme (map pack $ some (satisfy isAlphaNum) ) <?> "Identity"

-- ---------------------------------------------------------------- [ Commands ]

data DisplayPattern : Type where
  All      : DisplayPattern
  Outline  : DisplayPattern
  Metadata : DisplayPattern
  Heading  : String -> DisplayPattern

data EvalPattern : Type where
  Readability : EvalPattern
  TemplateAd  : EvalPattern
  SolutionApp : EvalPattern
  SifModel    : EvalPattern

public
data FriggCMD : Type where
  Display : DisplayPattern -> FriggCMD
  Convert : ConvPattern    -> FriggCMD
  Eval    : EvalPattern    -> FriggCMD
  Quit : FriggCMD
  Help : FriggCMD

showHelp : String
showHelp = """
Command                 | Description
------------------------|-------------------------------------------------------

:quit :q :exit          | Quit the repl
:? :help                | Show this help
"""

private
display : Parser FriggCMD
display = do
      string ":display"
      space
      opts <- displayOpts
      case opts of
        Just x  => pure $ Display x
        Nothing => (satisfy False)
    <?> "Display"
  where
    heading : Parser (Maybe DisplayPattern)
    heading = do
      w <- word
      pure $ (Just $ Heading w)

    displayOpts : Parser (Maybe DisplayPattern)
    displayOpts = (string "all"     *> pure $ Just All)
              <|> (string "outline" *> pure $ Just Outline)
              <|> (string "info"    *> pure $ Just Metadata)
              <|> heading
              <|> pure Nothing

private
convert : Parser FriggCMD
convert = do
    string ":convert"
    space
    fmt <- format
    case fmt of
      Just f  => pure $ ConvPattern fmt
      Nothing => (satisfy False)
  where
    format : Parser (Maybe ConvPattern)
    format = (string "" *> pure $ Just )
         <|> pure Nothing

private
evaluate : Parser FriggCMD
evaluate = do
    string ":evaluate"
    space
    evl <- aspect
    case evl of
      Just e  => pure $ EvalPattern e
      Nothing => (satisfy False)
  where
    aspect : Parser (Maybe ConvPattern)
    aspect = (string "" *> pure $ Just )
         <|> pure Nothing

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
