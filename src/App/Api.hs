{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module App.Api (
    IPv4T(..)
  , ObjectIdentifierT(..)
  , SnmpAPI
  , SnmpWithDocsAPI
  , docsApiBS
  , snmpAPI
  , snmpWithDocsAPI
  ) where

import           App.Parser              (parseIPv4, parseOID)
import           App.Snmp                (SnmpResponseT (..))

import           Data.ByteString.Lazy    (ByteString)
import qualified Data.Text               (pack)
import qualified Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Language.Asn.Types      (ObjectIdentifier (..))
import           Net.IPv4                (IPv4)
import           Servant                 (Proxy (..))
import           Servant.API             ((:<|>), (:>), Capture,
                                          FromHttpApiData (..), Get, JSON, Raw)
import           Servant.Docs

type SnmpAPI = "snmpget"  :> Capture "ip" IPv4T :> Capture "oid" ObjectIdentifierT :> Get '[JSON] SnmpResponseT

type SnmpWithDocsAPI = SnmpAPI :<|> Raw

newtype ObjectIdentifierT = ObjectIdentifierT ObjectIdentifier
instance FromHttpApiData ObjectIdentifierT where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . ObjectIdentifierT) (App.Parser.parseOID v)

newtype IPv4T = IPv4T IPv4
instance FromHttpApiData IPv4T where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . IPv4T) (App.Parser.parseIPv4 v)

snmpAPI :: Proxy SnmpAPI
snmpAPI = Proxy

snmpWithDocsAPI :: Proxy SnmpWithDocsAPI
snmpWithDocsAPI = Proxy

instance ToCapture (Capture "ip" IPv4T) where
  toCapture _ =
    DocCapture "ip"
               "(IPv4) the target for SNMP request"

instance ToCapture (Capture "oid" ObjectIdentifierT) where
  toCapture _ =
    DocCapture "oid"
               "SNMP Object Identifier to be requested"

docsApiBS :: ByteString
docsApiBS = encodeUtf8
       . Data.Text.Lazy.pack
       . markdown
       $ docsWithIntros [intro] snmpAPI
  where intro = DocIntro "Welcome" ["This is our super webservice's API.",  "Enjoy!"]
