module Crm.Server.ListParser (parseList) where

import Text.Parsec
import Data.Text hiding (foldr)

import Control.Applicative ((<*), (<$>), (*>))

import Crm.Shared.ServerRender (Markup(..))

type MyParsec a = Parsec String () a

parseList :: Text -> Either ParseError [Markup]
parseList t = fmap joinListElements $ parse listParser "" (unpack t)
  where
  joinListElements = foldr foldStep []
  foldStep (UnorderedList ul') (UnorderedList ul : rest) = (UnorderedList $ ul' ++ ul) : rest
  foldStep (element) (acc) = element : acc

listParser :: MyParsec [Markup]
listParser = do
  result <- many $ try listElementParser <|> plainLineParser 
  eof
  return result

listElementParser :: MyParsec Markup
listElementParser = UnorderedList <$> ((:[]) . pack <$> (string "- " *> line))

plainLineParser :: MyParsec Markup
plainLineParser = PlainText . pack <$> line


line :: MyParsec String
line = (many . noneOf $ "\r\n") <* eol
 
eol :: MyParsec ()
eol = do
  _ <- try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
  return ()
