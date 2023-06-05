module Hebele.Server.Internal.Algebra where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Hebele.Server.Internal.Types as Types


class MonadUser m where
  authenticate :: T.Text -> B.ByteString -> m (Maybe Types.User)


  find :: UUID -> m (Maybe Types.User)


  findByEmail :: T.Text -> m (Maybe Types.User)


  findCredentialsByEmail :: T.Text -> m (Maybe (Types.User, B.ByteString))
