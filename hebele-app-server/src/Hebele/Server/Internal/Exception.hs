{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hebele.Server.Internal.Exception where

import Control.Monad.Catch (Exception, MonadThrow (throwM))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack (HasCallStack)
import qualified Hasql.Pool
import qualified Hasql.Session


data HebeleError where
  HebeleError :: HasCallStack => T.Text -> HebeleError
  HebelePoolError :: HasCallStack => Hasql.Pool.UsageError -> HebeleError
  HebeleConnError :: HasCallStack => T.Text -> HebeleError
  HebeleQueryError :: HasCallStack => Hasql.Session.QueryError -> HebeleError


deriving instance Show HebeleError


instance Exception HebeleError


throw :: (MonadThrow m, HasCallStack) => T.Text -> m a
throw = throwM . HebeleError


throwPool :: (MonadThrow m, HasCallStack) => Hasql.Pool.UsageError -> m a
throwPool = throwM . HebelePoolError


throwConn :: (MonadThrow m, HasCallStack) => T.Text -> m a
throwConn = throwM . HebeleConnError


throwQuery :: (MonadThrow m, HasCallStack) => Hasql.Session.QueryError -> m a
throwQuery = throwM . HebeleQueryError


displayError :: HebeleError -> T.Text
displayError (HebeleError msg) = msg
displayError (HebelePoolError msg) = "Database Pool Usage Error: " <> T.pack (show msg)
displayError (HebeleConnError msg) = "Connection Error: " <> msg
displayError (HebeleQueryError qe) = "Query error: " <> displayQueryError qe


displayQueryError :: Hasql.Session.QueryError -> T.Text
displayQueryError (Hasql.Session.QueryError tmpl vars ce) =
  displayCommandError ce
    <> "\nStatement template: "
    <> TE.decodeUtf8 tmpl
    <> "\nStatement variables: \n  "
    <> T.intercalate "\n  " vars


displayCommandError :: Hasql.Session.CommandError -> T.Text
displayCommandError (Hasql.Session.ClientError Nothing) = "Unknown client error"
displayCommandError (Hasql.Session.ClientError (Just err)) = "Client error: " <> TE.decodeUtf8 err
displayCommandError (Hasql.Session.ResultError re) = "Result error: " <> displayCommandResultError re


displayCommandResultError :: Hasql.Session.ResultError -> T.Text
displayCommandResultError = T.pack . show -- TODO: Implement a better result error representation.
