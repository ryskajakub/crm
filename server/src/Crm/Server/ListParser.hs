module Crm.Server.ListParser (parseList) where

import Text.Parsec
import Data.Text

import Control.Applicative ((<*), (<$>), (*>))

import Crm.Shared.ServerRender (Markup(..))

type MyParsec a = Parsec String () a

parseList :: Text -> Either ParseError [Markup]
parseList t = parse listParser "" (unpack t)

listParser :: MyParsec [Markup]
listParser = do
  result <- many $ try listElementParser <|> plainLineParser 
  eof
  return result

line :: MyParsec String
line = (many . noneOf $ "\r\n") <* eol

listElementParser :: MyParsec Markup
listElementParser = UnorderedList <$> (many1 $ pack <$> (string "- " *> line))
 
eol :: MyParsec ()
eol = do
  _ <- try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
  return ()

plainLineParser :: MyParsec Markup
plainLineParser = PlainText . pack <$> line
