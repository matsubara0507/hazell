{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Bazel.Parser where

import           RIO                  hiding (many, some, try)
import qualified RIO.Map              as Map
import qualified RIO.Text             as Text

import           Bazel.Build          (BuildContent (..), BuildFile)
import           Bazel.Rule           (RuleArg (..))
import           Control.Monad        (MonadPlus)
import           Data.Functor         (($>))
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, digitChar, hspace, letterChar,
                                       newline, printChar, space, string)

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
buildRuleArgParser = do
  -- ToDo: no use sepBy for more exp
  args <- buildRuleArgParser' `sepBy1` try (space >> char '+' >> space)
  case args of
    [arg]         -> pure arg
    (arg : args') -> pure $ foldl' RuleArgAppend arg args'
    []            -> fail "Impossible pattern"
  where
    buildRuleArgParser'
        = buildRuleArgArrayParser
      <|> buildRuleArgDictParser
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

buildRuleArgDictParser :: Parser RuleArg
buildRuleArgDictParser = do
  char '{'
  space
  dict <- buildRuleDictMemberParser `sepAndEndBy` (comma, space >> char '}')
  pure $ RuleArgDict (Map.fromList dict)
  where
    buildRuleDictMemberParser :: Parser (String, RuleArg)
    buildRuleDictMemberParser = do
      key <- stringLitParser
      space
      char ':'
      space
      val <- buildRuleArgParser
      pure (key, val)

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
stringLitParser = char '"' >> stringLitParser' ""
  where
    stringLitParser' :: String -> Parser String
    stringLitParser' acc = do
      str <- Text.unpack <$> takeWhileP Nothing (\c -> c /= '"' && c /= '\\')
      let acc' = acc ++ str
      (char '"' >> pure acc') <|> (char '\\' >> char '"' >> stringLitParser' (acc' ++ "\""))

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
