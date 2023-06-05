{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Hebele.Server.Web.Api where

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
import qualified Hebele.Core as Core
import Hebele.Server.Internal.HebeleM (Env, HebeleM, runHebeleM)
import qualified Hebele.Server.Internal.Types as Types
import Hebele.Server.Web.Auth (authContext, mkAuthServerContext)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Wai.Logger
import Servant ((:<|>) ((:<|>)), (:>))
import qualified Servant
import qualified Servant.OpenApi
import qualified Servant.Swagger.UI


-- * Entrypoint


-- | Runs the Web server on the given port.
runWebServer :: Warp.Port -> Env -> IO ()
runWebServer port env = do
  putStrLn [i|Web server is now running on Port #{port}|]
  Wai.Logger.withStdoutLogger $ \logger -> do
    let settings = Warp.setPort port . Warp.setLogger logger $ Warp.defaultSettings
    Warp.runSettings settings (apiApplication env)


-- * Top-Level Application


-- | Provides the API application.
apiApplication :: Env -> Servant.Application
apiApplication env =
  Servant.serveWithContext apiProxy (mkAuthServerContext env) $
    Servant.hoistServerWithContext api authContext (liftIO . runHebeleM env) server
  where
    apiProxy = Servant.Proxy :: Servant.Proxy Api
    server = apiHandler


-- | API type definition.
type Api = ApiCore :<|> ApiDocs


-- | API definition.
api :: Servant.Proxy Api
api = Servant.Proxy


-- | API request handler.
apiHandler :: Servant.ServerT Api HebeleM
apiHandler = apiHandlerCore :<|> apiDocsServer


-- * API Endpoints


-- | Core API type definition.
type ApiCore = ApiCoreInformational


-- | Core API definition.
apiProxyCore :: Servant.Proxy ApiCore
apiProxyCore = Servant.Proxy


-- | Core API server implementation.
apiHandlerCore :: Servant.ServerT ApiCore HebeleM
apiHandlerCore = apiHandlerCoreInformational


-- ** Informational


-- | Core API type definition.
type ApiCoreInformational = ApiVersion :<|> ApiInfo :<|> ApiMe


-- | Core API definition.
apiProxyCoreInformational :: Servant.Proxy ApiCoreInformational
apiProxyCoreInformational = Servant.Proxy


-- | Core API server implementation.
apiHandlerCoreInformational :: Servant.ServerT ApiCoreInformational HebeleM
apiHandlerCoreInformational = apiHandlerVersion :<|> apiHandlerInfo :<|> apiHandlerMe


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


-- *** /me


-- | API type definition for @/me@.
type ApiMe =
  "me"
    :> Servant.AuthProtect "hebele-auth"
    :> Servant.Summary "Authenticated User Endpoint"
    :> Servant.Description "This endpoint returns authenticated user information."
    :> Servant.Get '[Servant.JSON] Types.User


-- | API definition for @/me@.
apiProxyMe :: Servant.Proxy ApiMe
apiProxyMe = Servant.Proxy


-- | API request handler for @/me@.
apiHandlerMe :: Servant.ServerT ApiMe HebeleM
apiHandlerMe = pure


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
apiDocsServer :: Servant.ServerT ApiDocs HebeleM
apiDocsServer = Servant.Swagger.UI.swaggerSchemaUIServerT openapi
