module App.Snmp
  ( SnmpResponse(..)
  , snmpGet
  , snmpGetBulkChildren
  , snmpGetBulkStep
  )
where

import           Control.Exception             (bracket)
import           Data.Aeson                    (ToJSON (..), Value (..), object,
                                                (.=))
import           Data.Bool                     (bool)
import           Data.ByteString               (ByteString)
import           Data.ByteString.Builder       (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Char8         (all, pack)
import           Data.ByteString.Lazy          (toStrict)
import           Data.Char                     (isPrint)
import           Data.Text                     (Text)
import qualified Data.Text                     (pack)
import qualified Data.Text.Encoding            (decodeUtf8)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   (fromList, toList)
import qualified Language.Asn.ObjectIdentifier as Asn (encodeText, fromList)
import           Language.Asn.Types            (ObjectIdentifier (..))
import           Net.IPv4                      (IPv4 (..))
import qualified Net.IPv4                      (encode)
import           Servant.Docs                  (ToSample (..), singleSample)
import           Snmp.Client                   (Config, Context (..),
                                                Credentials (..),
                                                CredentialsV2 (..),
                                                Destination (..), Session (..),
                                                SnmpException, closeSession,
                                                get', getBulkChildren',
                                                getBulkStep', openSession)
import           Snmp.Types                    (ApplicationSyntax (..),
                                                ObjectSyntax (..),
                                                SimpleSyntax (..))

type CommunityString = ByteString

snmpGet
  :: Config
  -> CommunityString
  -> ObjectIdentifier
  -> IPv4
  -> IO (Either SnmpException SnmpResponse)
snmpGet cnf com oid ip =
  withSession cnf
    (context com (destination ip))
    $ \cnx -> do
    v <- get' cnx oid
    return $ either (\l -> Left l) (\r -> Right $ SnmpResponse $ Data.Vector.fromList [(oid, r)]) v

snmpGetBulkStep
  :: Config
  -> CommunityString
  -> ObjectIdentifier
  -> IPv4
  -> Int
  -> IO (Either SnmpException SnmpResponse)
snmpGetBulkStep cnf com oid ip i = do
  withSession cnf
    (context com (destination ip))
    $ \cnx -> runGetBulk (getBulkStep' cnx i oid)

snmpGetBulkChildren
  :: Config
  -> CommunityString
  -> ObjectIdentifier
  -> IPv4
  -> Int
  -> IO (Either SnmpException SnmpResponse)
snmpGetBulkChildren cnf com oid ip i = do
  withSession cnf
    (context com (destination ip))
    $ \cnx -> runGetBulk (getBulkChildren' cnx i oid)

runGetBulk
  :: Monad m =>
  m (Either SnmpException (Vector (ObjectIdentifier,  ObjectSyntax)))
  -> m (Either SnmpException SnmpResponse)
runGetBulk f = f >>= \v -> return (eitherSnmpResponse v)

eitherSnmpResponse
  :: Either SnmpException (Vector (ObjectIdentifier, ObjectSyntax))
  -> Either SnmpException SnmpResponse
eitherSnmpResponse v = case v of
  Left  e  -> Left e
  Right v' -> Right (SnmpResponse v')

withSession
  :: Config
  -> (Session -> Context)
  -> (Context -> IO (Either SnmpException SnmpResponse))
  -> IO (Either SnmpException SnmpResponse)
withSession cnf f g = bracket (openSession cnf) closeSession
  $ \s -> g (f s)

snmpCredentials :: CommunityString -> Credentials
snmpCredentials com =
  CredentialsConstructV2 (CredentialsV2 { credentialsV2CommunityString = com })

context :: CommunityString -> Destination -> Session -> Context
context com d s = Context
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

stringifySimpleSyntax :: SimpleSyntax -> Text
stringifySimpleSyntax (SimpleSyntaxInteger  v) = Data.Text.pack (show v)
stringifySimpleSyntax (SimpleSyntaxObjectId v) = Asn.encodeText v
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
stringfyObjectIdentifier = Asn.encodeText

vectorToValue :: Vector (ObjectIdentifier, ObjectSyntax) -> [Value]
vectorToValue v = stringifyTuple <$> Data.Vector.toList v

stringifyTuple :: (ObjectIdentifier, ObjectSyntax) -> Value
stringifyTuple (a, b) =
  object [stringfyObjectIdentifier a .= stringifyObjectSyntax b]

type SnmpResponse' = (Vector (ObjectIdentifier,  ObjectSyntax))

newtype SnmpResponse = SnmpResponse SnmpResponse' deriving (Show)
instance ToJSON SnmpResponse where
  toJSON a = object [Data.Text.pack "SnmpResponse" .= vectorToValue (snmpResponse a)]

instance ToSample SnmpResponse where
  toSamples _ = singleSample (snmpResponseSample)

snmpResponseSample :: SnmpResponse
snmpResponseSample = SnmpResponse $ Data.Vector.fromList [(k, v)]
  where
    k = Asn.fromList [1, 3, 6, 1, 2, 1, 1, 1]
    v = ObjectSyntaxSimple $ SimpleSyntaxString $ Data.ByteString.Char8.pack "<<HW_REV: V1.0; VENDOR: ..."
snmpResponse :: SnmpResponse -> SnmpResponse'
snmpResponse (SnmpResponse a) = a
