{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hebele.Hubele.Server.Web.Auth where

import Control.Lens ((&), (.~), (<>~))
import Control.Monad ((>=>))
import qualified Crypto.Hash
import qualified Crypto.KDF.PBKDF2 as Crypto
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Char as C
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.TypeLits (KnownSymbol)
import Hebele.Hubele.Server.Internal.HebeleM (Env, runHebeleM)
import qualified Hebele.Hubele.Server.Internal.Types as Types
import qualified Network.Wai as Wai
import Servant ((:>))
import qualified Servant
import qualified Servant.OpenApi
import qualified Servant.Server.Experimental.Auth as Auth
import Control.Monad.IO.Class (MonadIO(..))
import Hebele.Hubele.Server.Internal.Algebra (MonadUser(authenticate))


-- | Authentication context proxy definition.
authContext :: Servant.Proxy '[Auth.AuthHandler Wai.Request Types.User]
authContext = Servant.Proxy


-- | Creates an authenticated server context.
mkAuthServerContext :: Env -> Servant.Context (Auth.AuthHandler Wai.Request Types.User ': '[])
mkAuthServerContext env = authHandler env Servant.:. Servant.EmptyContext


-- | Type definition for authenticated user parameter.
type instance Auth.AuthServerData (Servant.AuthProtect "hebele-hubele-auth") = Types.User


-- | Orphan instance of 'AuthProtect' for 'Servant.OpenApi.HasOpenApi'.
instance (KnownSymbol sym, Servant.OpenApi.HasOpenApi sub) => Servant.OpenApi.HasOpenApi (Servant.AuthProtect sym :> sub) where
  toOpenApi _ =
    Servant.OpenApi.toOpenApi (Servant.Proxy :: Servant.Proxy sub)
      & OpenApi.components . OpenApi.securitySchemes .~ securitySchemes
      & OpenApi.allOperations . OpenApi.security <>~ securityRequirements
      & OpenApi.setResponse 401 (pure $ mempty & OpenApi.description .~ "Authentication failed")
      & OpenApi.setResponse 403 (pure $ mempty & OpenApi.description .~ "Authorization failed")
    where
      authBasic = "Basic HTTP Authentication"
      schemeBasic = OpenApi.SecurityScheme (OpenApi.SecuritySchemeHttp OpenApi.HttpSchemeBasic) Nothing

      securityRequirements = [OpenApi.SecurityRequirement $ InsOrd.fromList [(authBasic, [])]]
      securitySchemes = OpenApi.SecurityDefinitions $ InsOrd.fromList [(authBasic, schemeBasic)]


-- | Authentication handler.
authHandler :: Env -> Auth.AuthHandler Wai.Request Types.User
authHandler env = Auth.mkAuthHandler (extractCredentials >=> authenticateX env)


-- | Attempts to perform authentication.
authenticateX :: Env -> (B.ByteString, B.ByteString) -> Servant.Handler Types.User
authenticateX env (u, p) = do
  let email = TE.decodeUtf8 u
  a <- liftIO $ runHebeleM env (authenticate email p)
  case a of
    Nothing -> Servant.throwError $ Servant.err401 {Servant.errBody = "Invalid authentication"}
    Just sp -> pure sp


-- | Attempts to extract the credentials from the given request.
extractCredentials :: Wai.Request -> Servant.Handler (BC.ByteString, B.ByteString)
extractCredentials req = case lookup "Authorization" $ Wai.requestHeaders req of
  Nothing -> Servant.throwError $ Servant.err401 {Servant.errBody = "No authorization information is provided."}
  Just ah -> case bimap (BC.map C.toUpper) BC.strip $ BC.span (' ' /=) ah of
    ("BASIC", x) -> pure . fmap (BC.drop 1) . BC.span (':' /=) . Base64.decodeLenient $ x
    (x, _) -> Servant.throwError $ Servant.err401 {Servant.errBody = BLC.append (BLC.pack "Unknown authentication scheme: ") (BLC.fromStrict x)}


-- | Hash a given password with a given salt.
--
-- >>> hashPassword "jjWyRNanEjU3" "password"
-- "pbkdf2_sha512$216000$jjWyRNanEjU3$WByp/9T5iGcbTMpI9RoiiB8tJgXmbBnEALGEF9wH2I0="
hashPassword :: T.Text -> T.Text -> T.Text
hashPassword salt password = do
  let value = TE.decodeUtf8 . Base64.encode $ Crypto.generate (Crypto.prfHMAC Crypto.Hash.SHA512) (Crypto.Parameters 216000 32) (TE.encodeUtf8 password) (TE.encodeUtf8 salt)
  "pbkdf2_sha512" <> "$216000$" <> salt <> "$" <> value
