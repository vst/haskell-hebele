{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hebele.Server.Internal.HebeleM where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import qualified Crypto.KDF.PBKDF2 as Crypto
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BC
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import GHC.Stack (HasCallStack)
import qualified Hasql.Pool
import qualified Hasql.Session
import qualified Hasql.Statement
import Hasql.TH (maybeStatement)
import Hebele.Server.Internal.Algebra (MonadUser (findCredentialsByEmail))
import qualified Hebele.Server.Internal.Algebra as Algebra
import Hebele.Server.Internal.Exception (throwPool)
import qualified Hebele.Server.Internal.Types as Types
import Text.Read (readMaybe)


-- * Environment


newtype Env = Env
  { envPool :: Hasql.Pool.Pool
  }


-- * HebeleM


newtype HebeleM a = HebeleM {unHebeleM :: ReaderT Env IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadCatch, MonadThrow)


instance Algebra.MonadUser HebeleM where
  authenticate :: T.Text -> B.ByteString -> HebeleM (Maybe Types.User)
  authenticate e p =
    findCredentialsByEmail e <&> maybe Nothing (authenticateUser p)


  find :: UUID -> HebeleM (Maybe Types.User)
  find x = runSession (Hasql.Session.statement x sqlFindUserById)


  findByEmail :: T.Text -> HebeleM (Maybe Types.User)
  findByEmail x = runSession (Hasql.Session.statement x sqlFindUserByEmail)


  findCredentialsByEmail :: T.Text -> HebeleM (Maybe (Types.User, B.ByteString))
  findCredentialsByEmail x = runSession (Hasql.Session.statement x sqlFindUserByEmailWithCredentials)


runHebeleM
  :: HasCallStack
  => Env
  -> HebeleM a
  -> IO a
runHebeleM env program = runReaderT (unHebeleM program) env


-- * Database Helpers


withPool :: (Monad m, MonadReader Env m) => (Hasql.Pool.Pool -> m a) -> m a
withPool act = asks envPool >>= act


runSession :: Hasql.Session.Session a -> HebeleM a
runSession session = do
  pool <- asks envPool
  liftIO (Hasql.Pool.use pool session) >>= either throwPool pure


sqlFindUserByEmail :: Hasql.Statement.Statement T.Text (Maybe Types.User)
sqlFindUserByEmail =
  dimap id (fmap fromResult) statement
  where
    fromResult (x, y, z) = Types.User x y z
    statement =
      [maybeStatement|
        SELECT
            "user"."id" :: uuid
          , "user"."email" :: text
          , "user"."fullname" :: text?
        FROM "user"
        WHERE "user"."email" = $1 :: text
        LIMIT 1
      |]


sqlFindUserByEmailWithCredentials :: Hasql.Statement.Statement T.Text (Maybe (Types.User, B.ByteString))
sqlFindUserByEmailWithCredentials =
  dimap id (fmap fromResult) statement
  where
    fromResult (x, y, z, p) = (Types.User x y z, TE.encodeUtf8 p)
    statement =
      [maybeStatement|
        SELECT
            "user"."id" :: uuid
          , "user"."email" :: text
          , "user"."fullname" :: text?
          , "user_credentials"."password" :: text
        FROM "user"
        JOIN "user_credentials" ON "user"."id" = "user_credentials"."user_id"
        WHERE "user"."email" = $1 :: text
        LIMIT 1
      |]


sqlFindUserById :: Hasql.Statement.Statement UUID (Maybe Types.User)
sqlFindUserById =
  dimap id (fmap fromResult) statement
  where
    fromResult (x, y, z) = Types.User x y z
    statement =
      [maybeStatement|
        SELECT
            "user"."id" :: uuid
          , "user"."email" :: text
          , "user"."fullname" :: text?
        FROM "user"
        WHERE "user"."email" = $1 :: uuid
        LIMIT 1
      |]


-- * Authentication Helpers


authenticateUser :: B.ByteString -> (Types.User, B.ByteString) -> Maybe Types.User
authenticateUser password (u, stored)
  | checkPassword password stored = Just u
  | otherwise = Nothing


checkPassword :: B.ByteString -> B.ByteString -> Bool
checkPassword password stored = case BC.split '$' stored of
  [algorithm, iterations, salt, pwdBase64] -> encode algorithm iterations salt == pwdBase64
  _ -> False
  where
    encode :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
    encode a i s = Base64.encode (getAlgo a (Crypto.Parameters (getIterations i) 32) password s)

    getIterations :: B.ByteString -> Int
    getIterations i = fromMaybe 1 $ readMaybe (BC.unpack i)

    getAlgo :: B.ByteString -> (Crypto.Parameters -> B.ByteString -> B.ByteString -> B.ByteString)
    getAlgo x
      | x == "pbkdf2_sha256" = Crypto.fastPBKDF2_SHA256
      | x == "pbkdf2_sha512" = Crypto.fastPBKDF2_SHA512
      | otherwise = \_ _ _ -> ""
