{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Bazel.Parser where

import Bazel.Build (BuildFile, BuildContent (..))
import Bazel.Rule (RuleArg (..))
import Control.Monad (MonadPlus)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, hspace, char, letterChar, digitChar, printChar, string)

parseBuildFile :: Text -> Maybe BuildFile
parseBuildFile = parseMaybe buildFileParser

type Parser = Parsec Void Text

buildFileParser :: Parser BuildFile
buildFileParser = many buildContentParser <* space

buildContentParser :: Parser BuildContent
buildContentParser
    = buildCommentParser
  <|> buildRuleParser
  <|> buildNewlineParser

buildCommentParser :: Parser BuildContent
buildCommentParser = do
  hspace
  char '#'
  comment <- many printChar
  newline
  pure $ BuildComment (Text.pack comment)

buildRuleParser :: Parser BuildContent
buildRuleParser = do
  name <- nameParser
  char '('
  space
  args <- argParser `sepAndEndBy` (comma, space >> char ')')
  optional newline
  pure $ BuildRule (Text.pack name) args
  where
    argParser = buildRuleArgWithNameParser <|> buildRuleArgWithoutNameParser

buildNewlineParser :: Parser BuildContent
buildNewlineParser = do
  newline
  pure BuildNewline

buildRuleArgWithNameParser :: Parser (Maybe String, RuleArg)
buildRuleArgWithNameParser = do
  name <- try $ nameParser <* space <* char '='
  space
  (Just name,) <$> buildRuleArgParser

buildRuleArgWithoutNameParser :: Parser (Maybe String, RuleArg)
buildRuleArgWithoutNameParser = (Nothing,) <$> buildRuleArgParser

buildRuleArgParser :: Parser RuleArg
buildRuleArgParser
    = buildRuleArgArrayParser
  <|> buildRuleArgBoolParser
  <|> buildRuleArgStringParser
  <|> buildRuleArgGlobParser
  <|> buildRuleArgConstParser

buildRuleArgStringParser :: Parser RuleArg
buildRuleArgStringParser = RuleArgString <$> stringLitParser

buildRuleArgBoolParser :: Parser RuleArg
buildRuleArgBoolParser =
  try $ string "True" $> RuleArgBool True
    <|> string "False" $> RuleArgBool False

buildRuleArgArrayParser :: Parser RuleArg
buildRuleArgArrayParser = do
  char '['
  space
  arr <- buildRuleArgParser `sepAndEndBy` (comma, space >> char ']')
  pure $ RuleArgArray arr

buildRuleArgConstParser :: Parser RuleArg
buildRuleArgConstParser = RuleArgConst <$> nameParser

buildRuleArgGlobParser :: Parser RuleArg
buildRuleArgGlobParser = do
  try $ string "glob(["
  path <- stringLitParser
  string "])"
  pure $ RuleArgGlob path

nameParser :: Parser String
nameParser = some (letterChar <|> digitChar <|> char '_')

comma :: Parser ()
comma = do
  hspace
  char ','
  space

stringLitParser :: Parser String
stringLitParser = do
  char '"'
  str <- takeWhile1P Nothing (/= '"') -- ToDo: escape "
  char '"'
  pure $ Text.unpack str

-- allow tail-sep
sepAndEndBy :: MonadPlus m => m a -> (m sep, m end) -> m [a]
sepAndEndBy p (sep, end) = go
  where
    go = do
      r <- optional p
      case r of
        Nothing -> end $> []
        Just x -> do
          s <- optional sep
          case s of
            Nothing -> end $> [x]
            Just _  -> (x:) <$> go
