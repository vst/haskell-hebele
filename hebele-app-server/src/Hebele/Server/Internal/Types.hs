{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hebele.Server.Internal.Types where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.UUID (UUID)


data User = User
  { userId :: !UUID
  , userEmail :: !T.Text
  , userFullname :: !(Maybe T.Text)
  }
  deriving (Eq, Show)


instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" $ \o ->
    User
      <$> (o .: "id")
      <*> (o .: "email")
      <*> (o .: "fullname")


instance Aeson.ToJSON User where
  toJSON User {..} =
    Aeson.object
      [ "id" .= userId
      , "email" .= userEmail
      , "fullname" .= userFullname
      ]


instance OpenApi.ToSchema User where
  declareNamedSchema _ = do
    schemaUuid <- OpenApi.declareSchemaRef (Proxy :: Proxy UUID)
    schemaText <- OpenApi.declareSchemaRef (Proxy :: Proxy T.Text)
    return . OpenApi.NamedSchema (Just "User") $
      mempty
        & OpenApi.type_ ?~ OpenApi.OpenApiObject
        & OpenApi.properties
          .~ [ ("id", schemaUuid)
             , ("email", schemaText)
             , ("fullname", schemaText)
             ]
        & OpenApi.required .~ ["id", "email"]
