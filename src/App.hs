{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
module App
  ( runApp
  )
where

import           App.Parser                               ( IPv4T(..)
                                                          , ObjectIdentifierT(..)
                                                          )
import           App.Settings                             ( Options(..) )
import           App.Snmp                                 ( SnmpResponseT )
import qualified App.Snmp                                 ( snmpGet )

import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Network.Wai.Handler.Warp                 ( run )
import           Servant                                  ( Application
                                                          , Handler
                                                          , Proxy(..)
                                                          , Server
                                                          , serve
                                                          )
import           Servant.API                              ( Capture
                                                          , Get
                                                          , JSON
                                                          , (:>)
                                                          )

type SnmpAPI = "snmpget"  :> Capture "ip" IPv4T :> Capture "oid" ObjectIdentifierT :> Get '[JSON] SnmpResponseT

snmpAPI :: Proxy SnmpAPI
snmpAPI = Proxy

snmpAPIServer :: Options -> Server SnmpAPI
snmpAPIServer = snmpGet

snmpGet :: Options -> IPv4T -> ObjectIdentifierT -> Handler SnmpResponseT
snmpGet o (IPv4T ip) (ObjectIdentifierT oid) = do
  v <- liftIO $ App.Snmp.snmpGet (roCommunity o) oid ip
  case v of
    Left  e -> error (show e)
    Right r -> return r

app :: Options -> Application
app o = serve snmpAPI (snmpAPIServer o)

runApp :: MonadIO m => Options -> m ()
runApp o = liftIO (run 8081 (app o))

-- newtype App m a = App {
--     unApp :: ReaderT Options m a
--     } deriving (Applicative, Functor, Monad, MonadReader Options, MonadIO)
--
-- runApp :: MonadIO m => ObjectIdentifier -> IPv4 -> Options -> m ()
-- runApp oid ip = runReaderT (unApp (app oid ip))
--
-- app :: (MonadIO m) => ObjectIdentifier -> IPv4 -> App m ()
-- app oid ip = do
--   com <- asks roCommunity
--   liftIO (App.Snmp.snmpGetBulk com oid ip 100000000 >>= \v -> printR v)
--   pure ()
--  where
--   printR v = case v of
--     Left  e -> print (show e)
--     Right r -> print (Data.Aeson.encode r)
