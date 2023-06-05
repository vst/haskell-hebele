-- | This module provides top-level definitions for the Web server application.
module Hebele.Server.Cli where

import Control.Applicative ((<**>))
import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Pool
import Hebele.Core (versionString)
import Hebele.Server.Internal.HebeleM (Env (..))
import qualified Hebele.Server.Web as Web
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))


-- * Program


-- | Runs the CLI program.
runCli :: IO ExitCode
runCli = join $ OA.execParser (OA.info opts desc)
  where
    opts = optProgram <**> optVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Hebele Web Server Application"
        <> OA.header ("hebele-app-server - Hebele Web Server Application v" <> versionString)
        <> OA.footer "This program provides Hebele Web server application"


-- | CLI options.
optProgram :: OA.Parser (IO ExitCode)
optProgram =
  program
    <$> OA.option
      OA.auto
      ( OA.short 'p' <> OA.long "port" <> OA.metavar "PORT" <> OA.value 3000 <> OA.showDefault <> OA.help "Port to run Web server on"
      )
    <*> OA.strOption (OA.short 'd' <> OA.long "database" <> OA.metavar "DATABASE" <> OA.help "Database URI")


-- | Program.
program :: Warp.Port -> T.Text -> IO ExitCode
program p dbUri = do
  pool <- Hasql.Pool.acquire 2 Nothing (TE.encodeUtf8 dbUri)
  Web.runWebServer p (Env pool) >> pure ExitSuccess


-- * Helpers


-- ** Option Parser Helpers


-- *** Version


-- | CLI option parser for application version.
optVersion :: OA.Parser (a -> a)
optVersion =
  OA.infoOption versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- ** Test Helpers


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser = OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as = case runParserTest p as of
  OA.Success _ -> pure (Right ())
  OA.Failure f -> pure (Left (show f))
  OA.CompletionInvoked _ -> pure (Right ())
