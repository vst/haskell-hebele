{-# LANGUAGE OverloadedStrings #-}

module Hebele.Domain.Internal where

import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base64 as Base64


-- $setup
--
-- >>> :set -XOverloadedStrings


-- | Type definition for plain text.
newtype PlainText = PlainText {unPlainText :: T.Text}
  deriving (Eq, Show)


-- | Constructor for 'PlainText'.
--
-- >>> mkPlainText "hebele"
-- PlainText {unPlainText = "hebele"}
mkPlainText :: T.Text -> PlainText
mkPlainText = PlainText


-- | 'IsString' instance for 'PlainText'.
--
-- >>> "hebele" :: PlainText
-- PlainText {unPlainText = "hebele"}
instance IsString PlainText where
  fromString = PlainText . T.pack


-- | Type definition for base64-encoded text.
newtype Base64Text = MkBase64Text {unBase64Text :: T.Text}
  deriving (Eq, Show)


-- | Smart constructor for 'Base64Text'.
--
-- >>> mkBase64Text "aGViZWxl"
-- Right (MkBase64Text {unBase64Text = "aGViZWxl"})
-- >>> mkBase64Text "not-a-valid-base64-encoded-text"
-- Left "Invalid Base64 encoded text"
mkBase64Text :: T.Text -> Either T.Text Base64Text
mkBase64Text x
  | Base64.isBase64 x = Right (MkBase64Text x)
  | otherwise = Left "Invalid Base64 encoded text"


-- | Converts a given 'PlainText' value into a 'Base64Text' value.
--
-- >>> encodePlainText "hebele"
-- MkBase64Text {unBase64Text = "aGViZWxl"}
encodePlainText :: PlainText -> Base64Text
encodePlainText = MkBase64Text . Base64.encodeBase64 . unPlainText


-- | Converts a given 'Base64Text' value into a 'PlainText' value.
--
-- >>> decodeBase64Text (encodePlainText "hebele")
-- PlainText {unPlainText = "hebele"}
decodeBase64Text :: Base64Text -> PlainText
decodeBase64Text = PlainText . Base64.decodeBase64Lenient . unBase64Text
