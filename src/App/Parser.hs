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
import           Language.Asn.ObjectIdentifier            ( fromList )
import           Language.Asn.Types                       ( ObjectIdentifier )

import           Net.IPv4                                 ( IPv4 )
import qualified Net.IPv4                                 ( parser )


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
