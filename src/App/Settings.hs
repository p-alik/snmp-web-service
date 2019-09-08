module App.Settings
  ( Options(..)
  , options
  )
where

import           Data.ByteString                 (ByteString)
import           Data.Semigroup                  ((<>))

import           Options.Applicative
import           Options.Applicative.Help.Pretty (text, vsep)

data Options = Options {
    roCommunity :: ByteString
  , timeout     :: Int
  , retries     :: Int
} deriving Show


parserOptions :: Parser Options
parserOptions = Options <$> strOption
  (long "readonly-community" <> metavar "SNMP-COMMUNITY-STRING" <> help
    "community for SNMP read only requests"
  )
  <*> oa "timeout" "SNMP request timeout in seconds."
  <*> oa "retry" "Number of retries to be used in SNMP request."
  where
    oa n h = option auto (long n <> metavar "1" <> value 1 <> hd (h:["Default 1"]))
    hd = helpDoc . Just . vsep . map text

parseOptions :: ParserInfo Options
parseOptions =
  info (parserOptions <**> helper) (progDesc "fetchs Cable modem data via SNMP")

options :: IO Options
options = execParser parseOptions
