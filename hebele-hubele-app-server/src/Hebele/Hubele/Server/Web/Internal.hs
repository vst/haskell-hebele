{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Hebele.Hubele.Server.Web.Internal where

import Control.Lens ((&), (.~), (?~))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import qualified Hebele.Hubele.Core as Core
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Wai.Logger
import Servant ((:<|>) ((:<|>)), (:>))
import qualified Servant
import qualified Servant.OpenApi
import qualified Servant.Swagger.UI


-- * Entrypoint


-- | Runs the Web server on the given port.
runWebServer :: Warp.Port -> IO ()
runWebServer port = do
  putStrLn [i|Web server is now running on Port #{port}|]
  Wai.Logger.withStdoutLogger $ \logger -> do
    let settings = Warp.setPort port . Warp.setLogger logger $ Warp.defaultSettings
    Warp.runSettings settings apiApplication


-- * Top-Level Application


-- | Provides the API application.
apiApplication :: Servant.Application
apiApplication = Servant.serve apiProxy apiHandler


-- | API type definition.
type Api = ApiCore :<|> ApiDocs


-- | API definition.
apiProxy :: Servant.Proxy Api
apiProxy = Servant.Proxy


-- | API request handler.
apiHandler :: Servant.Server Api
apiHandler = apiHandlerCore :<|> apiDocsServer


-- * API Endpoints


-- | Core API type definition.
type ApiCore = ApiCoreInformational


-- | Core API definition.
apiProxyCore :: Servant.Proxy ApiCore
apiProxyCore = Servant.Proxy


-- | Core API server implementation.
apiHandlerCore :: Servant.Server ApiCore
apiHandlerCore = apiHandlerCoreInformational


-- ** Informational


-- | Core API type definition.
type ApiCoreInformational = ApiVersion :<|> ApiInfo


-- | Core API definition.
apiProxyCoreInformational :: Servant.Proxy ApiCoreInformational
apiProxyCoreInformational = Servant.Proxy


-- | Core API server implementation.
apiHandlerCoreInformational :: Servant.Server ApiCoreInformational
apiHandlerCoreInformational = apiHandlerVersion :<|> apiHandlerInfo


-- *** /version


-- | API type definition for @/version@.
type ApiVersion =
  "version"
    :> Servant.Summary "Version Endpoint"
    :> Servant.Description "This endpoint returns application version."
    :> Servant.Get '[Servant.JSON] VersionData


-- | API definition for @/version@.
apiProxyVersion :: Servant.Proxy ApiVersion
apiProxyVersion = Servant.Proxy


-- | API request handler for @/version@.
apiHandlerVersion :: Monad m => m VersionData
apiHandlerVersion = pure (VersionData Core.versionText)


-- | Data definition of version data.
newtype VersionData = VersionData
  { unVersionData :: T.Text
  }
  deriving (Generic, Aeson.ToJSON, OpenApi.ToSchema)


-- *** /info


-- | API type definition for @/info@.
type ApiInfo =
  "info"
    :> Servant.Summary "Information Endpoint"
    :> Servant.Description "This endpoint returns application information."
    :> Servant.Get '[Servant.JSON] InfoData


-- | API definition for @/info@.
apiProxyInfo :: Servant.Proxy ApiInfo
apiProxyInfo = Servant.Proxy


-- | API request handler for @/info@.
apiHandlerInfo :: MonadIO m => m InfoData
apiHandlerInfo = InfoData Core.versionText <$> liftIO Time.getCurrentTime


-- | Data definition of information data.
data InfoData = InfoData
  { infoDataVersion :: T.Text
  , infoDataTimestamp :: !Time.UTCTime
  }
  deriving (Generic)


instance Aeson.ToJSON InfoData where
  toJSON InfoData {..} =
    Aeson.object
      [ "version" .= infoDataVersion
      , "timestamp" .= infoDataTimestamp
      ]


instance OpenApi.ToSchema InfoData where
  declareNamedSchema _ = do
    schemaText <- OpenApi.declareSchemaRef (Proxy :: Proxy T.Text)
    schemaUtcTime <- OpenApi.declareSchemaRef (Proxy :: Proxy Time.UTCTime)
    return . OpenApi.NamedSchema (Just "InfoData") $
      mempty
        & OpenApi.type_ ?~ OpenApi.OpenApiObject
        & OpenApi.properties
          .~ [ ("version", schemaText)
             , ("timestamp", schemaUtcTime)
             ]
        & OpenApi.required .~ ["version", "timestamp"]


-- * API Documentation


-- | API type definition for serving both OpenAPI specification file and Swagger UI.
type ApiDocs = "docs" :> Servant.Swagger.UI.SwaggerSchemaUI "swagger" "openapi.json"


-- | OpenAPI specification for the API application.
openapi :: OpenApi.OpenApi
openapi =
  Servant.OpenApi.toOpenApi apiProxyCore
    & OpenApi.info . OpenApi.title .~ "API Documentation"
    & OpenApi.info . OpenApi.description ?~ "API Documentation Details."
    & OpenApi.info . OpenApi.termsOfService ?~ "https://example.com/terms-of-service"
    & OpenApi.info . OpenApi.contact ?~ contact
    & OpenApi.info . OpenApi.license ?~ license
    & OpenApi.info . OpenApi.version .~ Core.versionText
    & OpenApi.tags .~ [tagInformational]
    & OpenApi.applyTagsFor (Servant.OpenApi.subOperations apiProxyCoreInformational apiProxyCore) [tagInformational]
  where
    license = "This work is licensed under some terms. Contact us for further details." & OpenApi.url ?~ OpenApi.URL "https://example.com"
    contact =
      mempty
        & OpenApi.name ?~ "Example Pte Ltd"
        & OpenApi.url ?~ OpenApi.URL "https://example.com"
        & OpenApi.email ?~ "info@example.com"

    -- Tag Definitions:
    tagInformational = "Informational" & OpenApi.description ?~ "Collection of informational API endpoints"


-- | API documentation server implementation.
apiDocsServer :: Servant.Server ApiDocs
apiDocsServer = Servant.Swagger.UI.swaggerSchemaUIServer openapi
