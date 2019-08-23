module App.Settings
  ( Options(..)
  , options
  )
where

import           Data.ByteString                          ( ByteString )
import           Data.Semigroup                           ( (<>) )

import           Options.Applicative

data Options = Options {
    roCommunity :: ByteString
} deriving Show


parserOptions :: Parser Options
parserOptions = Options <$> strOption
  (long "readonly-community" <> metavar "SNMP-COMMUNITY-STRING" <> help
    "community for SNMP read only requests"
  )

parseOptions :: ParserInfo Options
parseOptions =
  info (parserOptions <**> helper) (progDesc "fetchs Cable modem data via SNMP")

options :: IO Options
options = execParser parseOptions
