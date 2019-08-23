module App.Snmp
  ( SnmpResponseT
  , snmpGet
  , snmpGetBulk
  , vectorToValue
  )
where

import           Control.Exception                        ( bracket )
import           Data.Aeson                               ( ToJSON(..)
                                                          , Value(..)
                                                          , object
                                                          , (.=)
                                                          )
import           Data.Bool                                ( bool )
import           Data.ByteString                          ( ByteString )
import           Data.ByteString.Builder                  ( byteStringHex
                                                          , toLazyByteString
                                                          )
import           Data.ByteString.Lazy                     ( toStrict )
import qualified Data.ByteString.Char8                    ( all )
import           Data.Char                                ( isPrint )
import           Data.Text                                ( Text )
import qualified Data.Text                                ( pack )
import qualified Data.Text.Encoding                       ( decodeUtf8 )
import           Data.Vector                              ( Vector )
import qualified Data.Vector                              ( toList )
import           Language.Asn.ObjectIdentifier            ( encodeText )
import           Language.Asn.Types                       ( ObjectIdentifier(..)
                                                          )
import           Net.IPv4                                 ( IPv4(..) )
import qualified Net.IPv4                                 ( encode )
import           Snmp.Client                              ( Config(..)
                                                          , Context(..)
                                                          , Credentials(..)
                                                          , CredentialsV2(..)
                                                          , Destination(..)
                                                          , Session(..)
                                                          , SnmpException
                                                          , closeSession
                                                          , getBulkStep'
                                                          , openSession
                                                          )
import           Snmp.Types                               ( ApplicationSyntax(..)
                                                          , ObjectSyntax(..)
                                                          , SimpleSyntax(..)
                                                          )

type CommunityString = ByteString

snmpGet
  :: CommunityString
  -> ObjectIdentifier
  -> IPv4
  -> IO (Either SnmpException SnmpResponseT)
snmpGet com oid ip = snmpGetBulk com oid ip 1

snmpGetBulk'
  :: CommunityString
  -> ObjectIdentifier
  -> IPv4
  -> Int
  -> IO
       ( Either
           SnmpException
           (Vector (ObjectIdentifier, ObjectSyntax))
       )
snmpGetBulk' com oid ip i = bracket (openSession config) closeSession
  $ \s -> getBulkStep' (context s com (destination ip)) i oid

snmpGetBulk
  :: CommunityString
  -> ObjectIdentifier
  -> IPv4
  -> Int
  -> IO (Either SnmpException SnmpResponseT)
snmpGetBulk com oid ip i = do
  v <- snmpGetBulk' com oid ip i
  return (eitherSnmpResponse v)

eitherSnmpResponse
  :: Either SnmpException (Vector (ObjectIdentifier, ObjectSyntax))
  -> Either SnmpException SnmpResponseT
eitherSnmpResponse v = case v of
  Left  e  -> Left e
  Right v' -> Right (SnmpResponseT v')

config :: Config
config = Config
  { configSocketPoolSize      = 1
  , configTimeoutMicroseconds = 1000000
  , configRetries             = 1
  }

snmpCredentials :: CommunityString -> Credentials
snmpCredentials com =
  CredentialsConstructV2 (CredentialsV2 { credentialsV2CommunityString = com })

context :: Session -> CommunityString -> Destination -> Context
context s com d = Context
  { contextSession     = s
  , contextDestination = d
  , contextCredentials = snmpCredentials com
  }

destination :: IPv4 -> Destination
destination ip = Destination { destinationHost = ip, destinationPort = 161 }

stringifyObjectSyntax :: ObjectSyntax -> Text
stringifyObjectSyntax (ObjectSyntaxApplication o) =
  stringifyApplicationSyntax o
stringifyObjectSyntax (ObjectSyntaxSimple o) = stringifySimpleSyntax o
-- stringifyObjectSyntax (ObjectSyntaxSimple o) = Data.Text.pack (show o)

stringifySimpleSyntax :: SimpleSyntax -> Text
stringifySimpleSyntax (SimpleSyntaxInteger  v) = Data.Text.pack (show v)
stringifySimpleSyntax (SimpleSyntaxObjectId v) = Data.Text.pack (show v)
stringifySimpleSyntax (SimpleSyntaxString   v) = showSimplySyntaxString v
 where
  showSimplySyntaxString v' = Data.Text.Encoding.decodeUtf8 $ bool
    ((toStrict . toLazyByteString . byteStringHex) v')
    v'
    (Data.ByteString.Char8.all isPrint v')

stringifyApplicationSyntax :: ApplicationSyntax -> Text
stringifyApplicationSyntax (ApplicationSyntaxIpAddress o) =
  Net.IPv4.encode (IPv4 { getIPv4 = o })
stringifyApplicationSyntax o = Data.Text.pack (show o)

stringfyObjectIdentifier :: ObjectIdentifier -> Text
stringfyObjectIdentifier = encodeText

vectorToValue :: Vector (ObjectIdentifier, ObjectSyntax) -> [Value]
vectorToValue v = stringifyTuple <$> Data.Vector.toList v

stringifyTuple :: (ObjectIdentifier, ObjectSyntax) -> Value
stringifyTuple (a, b) =
  object [stringfyObjectIdentifier a .= stringifyObjectSyntax b]

type SnmpResponse = (Vector (ObjectIdentifier,  ObjectSyntax))

newtype SnmpResponseT = SnmpResponseT SnmpResponse deriving (Show)
instance ToJSON SnmpResponseT where
  toJSON a = object [Data.Text.pack "SnmpResponseT" .= vectorToValue (snmpResponse a)]

snmpResponse :: SnmpResponseT -> SnmpResponse
snmpResponse (SnmpResponseT a) = a
