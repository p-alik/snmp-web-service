module App.Parser where

import           Control.Applicative                      ( (<|>) )
import           Data.Attoparsec.Text                     ( Parser
                                                          , char
                                                          , decimal
                                                          , endOfInput
                                                          , parseOnly
                                                          , sepBy
                                                          )

import           Data.Text                                ( Text )
import qualified Data.Text                                ( pack )
import           Language.Asn.ObjectIdentifier            ( fromList )
import           Language.Asn.Types                       ( ObjectIdentifier )

import           Net.IPv4                                 ( IPv4 )
import qualified Net.IPv4                                 ( parser )

import           Servant.API                              ( FromHttpApiData(..)
                                                          )

newtype ObjectIdentifierT = ObjectIdentifierT ObjectIdentifier
instance FromHttpApiData ObjectIdentifierT where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . ObjectIdentifierT) (App.Parser.parseOID v)

newtype IPv4T = IPv4T IPv4
instance FromHttpApiData IPv4T where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . IPv4T) (App.Parser.parseIPv4 v)

parserIPv4 :: Parser IPv4
parserIPv4 = Net.IPv4.parser

parseIPv4 :: Text -> Either String IPv4
parseIPv4 = parseOnly parserIPv4

parserOID :: Parser [Word]
parserOID = parserWord `sepBy` char '.'

parserWord :: Parser Word
parserWord = decimal

parseWords :: Text -> Either String [Word]
parseWords = parseOnly $ ((char '.' *> parserOID) <|> parserOID) <* endOfInput

parseOID :: Text -> Either String ObjectIdentifier
parseOID v = fromList <$> (parseWords v)
