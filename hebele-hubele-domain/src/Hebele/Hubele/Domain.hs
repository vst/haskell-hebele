-- | This module provides various domain definitions.
--
-- In particular, our domain consists of plain and base64-encoded textual values
-- and conversion functions between them.
--
-- >>> :set -XOverloadedStrings
-- >>> let textPlain = PlainText "hebele-hubele"
-- >>> textPlain
-- PlainText {unPlainText = "hebele-hubele"}
-- >>> let textBase64 = encodePlainText textPlain
-- >>> textBase64
-- MkBase64Text {unBase64Text = "aGViZWxlLWh1YmVsZQ=="}
-- >>> decodeBase64Text textBase64
-- PlainText {unPlainText = "hebele-hubele"}
module Hebele.Hubele.Domain (
  -- * Plain Text
  PlainText (..),
  mkPlainText,

  -- * Base64-Encoded Text
  Base64Text (unBase64Text),
  mkBase64Text,

  -- * Conversions
  encodePlainText,
  decodeBase64Text,
) where

import Hebele.Hubele.Domain.Internal (
  Base64Text (unBase64Text),
  PlainText (..),
  decodeBase64Text,
  encodePlainText,
  mkBase64Text,
  mkPlainText,
 )

