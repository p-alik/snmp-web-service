{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module App.Api (
    IPv4'(..)
  , ObjectIdentifier'(..)
  , SnmpAPI
  , SnmpWithDocsAPI
  , Step(..)
  , docsApiBS
  , snmpAPI
  , snmpWithDocsAPI
  ) where

import           App.Parser              (parseIPv4, parseOID, parseWord)
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

type SnmpAPI = "get"  :> Capture "ip" IPv4' :> Capture "oid" ObjectIdentifier' :> Get '[JSON] SnmpResponseT
          :<|> "getBulkStep" :> Capture "ip" IPv4' :> Capture "oid" ObjectIdentifier' :> Capture "step" Step :> Get '[JSON] SnmpResponseT

type SnmpWithDocsAPI = SnmpAPI :<|> Raw

newtype Step = Step Int
instance FromHttpApiData Step where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . Step . fromIntegral) (App.Parser.parseWord v)

newtype ObjectIdentifier' = ObjectIdentifier' ObjectIdentifier
instance FromHttpApiData ObjectIdentifier' where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . ObjectIdentifier') (App.Parser.parseOID v)

newtype IPv4' = IPv4' IPv4
instance FromHttpApiData IPv4' where
  parseQueryParam v = either (Left . Data.Text.pack) (Right . IPv4') (App.Parser.parseIPv4 v)

snmpAPI :: Proxy SnmpAPI
snmpAPI = Proxy

snmpWithDocsAPI :: Proxy SnmpWithDocsAPI
snmpWithDocsAPI = Proxy

instance ToCapture (Capture "ip" IPv4') where
  toCapture _ =
    DocCapture "ip"
               "(IPv4) the target for SNMP request"

instance ToCapture (Capture "oid" ObjectIdentifier') where
  toCapture _ =
    DocCapture "oid"
               "SNMP Object Identifier to be requested"

instance ToCapture (Capture "step" Step) where
  toCapture _ =
    DocCapture "step"
               "number of steps to be proceeded beginning by provided OID"
docsApiBS :: ByteString
docsApiBS = encodeUtf8
       . Data.Text.Lazy.pack
       . markdown
       $ docsWithIntros [intro] snmpAPI
  where intro = DocIntro "Welcome" ["This is our super webservice's API.",  "Enjoy!"]
