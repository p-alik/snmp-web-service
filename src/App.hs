{-# LANGUAGE OverloadedStrings #-}
module App
  ( runApp
  )
where

import           App.Api
import           App.Settings                              (Options (..))
import           App.Snmp                                  (CommunityString,
                                                            IPv4,
                                                            ObjectIdentifier,
                                                            SnmpException,
                                                            SnmpResponse (..),
                                                            snmpGet,
                                                            snmpGetBulkChildren,
                                                            snmpGetBulkStep)

import qualified Data.ByteString.Lazy.Char8                as B (pack)
import           Data.Default                              (def)

import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Control.Monad.Trans.Reader                (ReaderT, asks,
                                                            runReaderT)
import           Network.HTTP.Types                        (ok200)
import           Network.Wai                               (Middleware,
                                                            Response,
                                                            responseLBS)
import           Network.Wai.Handler.Warp                  (defaultSettings,
                                                            runSettings,
                                                            setPort)
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (..),
                                                            mkRequestLogger,
                                                            outputFormat)
import           Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import           Servant                                   (Application,
                                                            Handler, ServerT,
                                                            serve, throwError)
import           Servant.API
import           Servant.Server                            (ServantErr (errBody),
                                                            Tagged (..), err404,
                                                            hoistServer)
import           Snmp.Client                               (Config (..))

type AppM = ReaderT Options Handler

serverSnmp :: ServerT SnmpAPI AppM
serverSnmp = get :<|> getBulkStep :<|> getBulkChildren

serverSnmpWithDocsAPI :: ServerT SnmpWithDocsAPI AppM
serverSnmpWithDocsAPI = serverSnmp :<|> Tagged serveDocs
  where
    serveDocs :: p -> (Response -> t) -> t
    serveDocs _ respond = respond $ responseLBS ok200 [plain] docsApiBS
      where
      plain = ("Content-Type",  "text/plain")

get :: IPv4' -> ObjectIdentifier' -> AppM SnmpResponse
get (IPv4' ip) (ObjectIdentifier' oid) = do
  com <- asks roCommunity
  cnf <-  snmpClientConfig
  v <- liftIO $ snmpGet cnf com oid ip
  case v of
    Left  e -> throwError (err404 {errBody = B.pack $ show e})
    Right r -> return r

getBulkStep :: IPv4' -> ObjectIdentifier' -> Step -> AppM SnmpResponse
getBulkStep = getBulk snmpGetBulkStep

getBulkChildren :: IPv4' -> ObjectIdentifier' -> Step -> AppM SnmpResponse
getBulkChildren = getBulk snmpGetBulkChildren

getBulk ::
    (Config
    -> CommunityString
    -> ObjectIdentifier
    -> IPv4
    -> Int
    -> IO (Either SnmpException SnmpResponse))
    -> IPv4' -> ObjectIdentifier' -> Step -> AppM SnmpResponse
getBulk f (IPv4' ip) (ObjectIdentifier' oid) (Step i) = do
  com <- asks roCommunity
  cnf <-  snmpClientConfig
  v <- liftIO $ f cnf com oid ip i
  case v of
    Left  e -> throwError (err404 {errBody = B.pack $ show e})
    Right r -> return r

app :: Options -> Application
app o = serve snmpWithDocsAPI $ hoistServer snmpWithDocsAPI nt serverSnmpWithDocsAPI
  where
    nt :: AppM a -> Handler a
    nt x = runReaderT x o

runApp :: MonadIO m => Options -> m ()
runApp o = do
  l <- liftIO jsonRequestLogger
  let set = setPort 8081 defaultSettings
  liftIO (runSettings set $ l $ (app o))
  where
    jsonRequestLogger :: IO Middleware
    jsonRequestLogger =
      mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

snmpClientConfig :: AppM Config
snmpClientConfig = do
  t <- asks timeout
  r <- asks retries
  return $ Config
    { configSocketPoolSize      = 1
    , configTimeoutMicroseconds = t * 1000000
    , configRetries             = r
    }
