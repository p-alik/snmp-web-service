{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module App
  ( runApp
  )
where

import           App.Parser                                (IPv4T (..), ObjectIdentifierT (..))
import           App.Settings                              (Options (..))
import           App.Snmp                                  (SnmpResponseT)
import qualified App.Snmp                                  (snmpGet)

import qualified Data.ByteString.Lazy.Char8                as B (pack)
import           Data.Default                              (def)

import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Control.Monad.Trans.Reader                (ReaderT, ask,
                                                            runReaderT)
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (defaultSettings,
                                                            runSettings,
                                                            setPort)
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (..),
                                                            mkRequestLogger,
                                                            outputFormat)
import           Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import           Servant                                   (Application,
                                                            Handler, Proxy (..),
                                                            ServerT, serve,
                                                            throwError)
import           Servant.API                               ((:>), Capture, Get,
                                                            JSON)
import           Servant.Server                            (ServantErr (errBody),
                                                            err404, hoistServer)

type SnmpAPI = "snmpget"  :> Capture "ip" IPv4T :> Capture "oid" ObjectIdentifierT :> Get '[JSON] SnmpResponseT

type AppM = ReaderT Options Handler

snmpAPI :: Proxy SnmpAPI
snmpAPI = Proxy

snmpAPIServer :: ServerT SnmpAPI AppM
snmpAPIServer = snmpGet

snmpGet :: IPv4T -> ObjectIdentifierT -> AppM SnmpResponseT
snmpGet (IPv4T ip) (ObjectIdentifierT oid) = do
  o <- ask
  v <- liftIO $ App.Snmp.snmpGet (roCommunity o) oid ip
  case v of
    Left  e -> throwError (err404 {errBody = B.pack $ show e})
    Right r -> return r

nt :: Options -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Options -> Application
app o = serve snmpAPI (hoistServer snmpAPI (nt o) snmpAPIServer)

runApp :: MonadIO m => Options -> m ()
runApp o = do
  l <- liftIO jsonRequestLogger
  let set = setPort 8081 defaultSettings
  -- liftIO (run 8081 (app o))
  liftIO (runSettings set $ l $ (app o))

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
