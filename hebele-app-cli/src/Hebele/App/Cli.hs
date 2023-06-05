{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides top-level definitions for the CLI program.
module Hebele.App.Cli where

import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.List
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Hebele.Core (versionString)
import qualified Hebele.Domain as Domain
import qualified Options.Applicative as OA
import qualified Options.Applicative.Help as OA.Help
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)


-- * Program


-- | Runs the CLI program.
runCli :: IO ExitCode
runCli = join $ OA.execParser (OA.info opts desc)
  where
    opts = optProgram <**> optVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Hebele CLI - Top Level Commands"
        <> header
        <> OA.footerDoc (Just msg)


-- | CLI option parser for top-level commands.
optProgram :: OA.Parser (IO ExitCode)
optProgram = commandInfo <|> commandCodec


-- | Footer message for the top-level commands help page.
msg :: OA.Help.Doc
msg =
  [i|
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc vitae nibh
vitae elit faucibus efficitur in id turpis. Aliquam accumsan purus pulvinar
risus euismod, non facilisis ligula pellentesque. Integer pulvinar turpis
lacus, id accumsan massa sodales ac. Morbi lacinia lorem nec lacus ultrices
venenatis. Interdum et malesuada fames ac ante ipsum primis in faucibus.
Aenean convallis massa non arcu vestibulum, in aliquam arcu sollicitudin.
Fusce porta nisi maximus mi tempus, nec molestie neque semper. Mauris
commodo neque ut lectus faucibus, quis efficitur nisi dignissim. Vivamus ut
ante in nisi aliquam interdum et sit amet felis. Morbi imperdiet mauris in
sapien suscipit, at porta odio sagittis.
  |]
    <> [i|
Vivamus ac massa eget est congue pulvinar. Proin nec ligula augue. Nam
facilisis tristique nunc fermentum maximus. Vestibulum turpis magna, dapibus
ut urna sit amet, congue semper tellus. Proin scelerisque facilisis urna
vitae ultrices. Etiam ac viverra est. In hac habitasse platea dictumst. Cras
fringilla eros ac nisl tempus, nec suscipit metus gravida.
  |]


-- * Commands


-- ** info


-- | Provides top-level command for information related subcommands.
commandInfo :: OA.Parser (IO ExitCode)
commandInfo =
  mkSubCommand
    "info"
    parser
    "Information Commands"
    "This command provides information subcommands."
  where
    parser = subcommandInfoVersion


-- *** info version


-- | @version@ subcommand definition.
subcommandInfoVersion :: OA.Parser (IO ExitCode)
subcommandInfoVersion =
  mkSubCommand
    "version"
    parser
    "Print Version Information"
    "This command prints version information in one of the available formats."
  where
    parser = programVersion <$> optFormat


-- | @version@ subcommand program.
programVersion :: OutputFormat -> IO ExitCode
programVersion f =
  putStrLn output >> pure ExitSuccess
  where
    output = case f of
      OutputFormatText -> versionString
      OutputFormatJson -> [i|{"version": "#{versionString}"}|]
      OutputFormatYaml -> [i|version: "#{versionString}"|]


-- ** codec


-- | Provides top-level command for codec related subcommands.
commandCodec :: OA.Parser (IO ExitCode)
commandCodec =
  mkSubCommand
    "codec"
    parser
    "Base64 Codec Commands"
    "This command provides base64 codec subcommands."
  where
    parser = subcommandCodecEncode <|> subcommandCodecDecode


-- *** codec encode


-- | @encode@ subcommand definition.
subcommandCodecEncode :: OA.Parser (IO ExitCode)
subcommandCodecEncode =
  mkSubCommand
    "encode"
    parser
    "Encode Text"
    "This command base64-encodes a given text. Text can be given via the command line option or standard input."
  where
    parser =
      programCodecEncode
        <$> OA.optional (OA.strOption $ OA.short 't' <> OA.long "text" <> OA.metavar "TEXT" <> OA.help "Text to encode")


-- | @encode@ subcommand program.
programCodecEncode :: Maybe T.Text -> IO ExitCode
programCodecEncode mT = do
  textPlain <- maybe readPlainText (pure . Domain.PlainText) mT
  let textBase64 = Domain.encodePlainText textPlain
  putStrLn (T.unpack (Domain.unBase64Text textBase64))
  pure ExitSuccess
  where
    readPlainText = Domain.PlainText . T.pack <$> getContents


-- *** codec decode


-- | @decode@ subcommand definition.
subcommandCodecDecode :: OA.Parser (IO ExitCode)
subcommandCodecDecode =
  mkSubCommand
    "decode"
    parser
    "Decode Text"
    "This command decodes a given base64-encoded text. Text can be given via the command line option or standard input."
  where
    parser =
      programCodecDecode
        <$> OA.optional (OA.strOption $ OA.short 't' <> OA.long "text" <> OA.metavar "TEXT" <> OA.help "Text to decode")


-- | @decode@ subcommand program.
programCodecDecode :: Maybe T.Text -> IO ExitCode
programCodecDecode mT = do
  eTextBase64 <- Domain.mkBase64Text . T.strip <$> maybe readText pure mT
  case eTextBase64 of
    Left err -> putStrLn (T.unpack err) >> pure (ExitFailure 1)
    Right st -> do
      let textPlainText = Domain.decodeBase64Text st
      hPutStrLn stderr (T.unpack (Domain.unPlainText textPlainText))
      pure ExitSuccess
  where
    readText = T.pack <$> getContents


-- * Helpers


-- ** Option Parser Helpers


-- *** Output Format


-- | Data definition for possible output formats.
--
-- >>> [minBound..maxBound] :: [OutputFormat]
-- [OutputFormatText,OutputFormatJson,OutputFormatYaml]
data OutputFormat
  = OutputFormatText
  | OutputFormatJson
  | OutputFormatYaml
  deriving (Bounded, Enum, Eq, Show)


-- | Parses 'OutputFormat' from a given 'String'.
parseFormat :: String -> Either String OutputFormat
parseFormat "text" = Right OutputFormatText
parseFormat "json" = Right OutputFormatJson
parseFormat "yaml" = Right OutputFormatYaml
parseFormat x = Left ("Invalid format: " <> x)


-- | Represents 'OutputFormat' as a 'String' value.
formatToString :: OutputFormat -> String
formatToString OutputFormatText = "text"
formatToString OutputFormatJson = "json"
formatToString OutputFormatYaml = "yaml"


-- | CLI option parser for output format.
--
-- >>> runParserTest optFormat ["-f", "text"]
-- Success OutputFormatText
-- >>> runParserTest optFormat ["--format", "text"]
-- Success OutputFormatText
-- >>> runParserTest optFormat ["-f", "json"]
-- Success OutputFormatJson
-- >>> runParserTest optFormat ["--format", "json"]
-- Success OutputFormatJson
-- >>> runParserTest optFormat ["-f", "yaml"]
-- Success OutputFormatYaml
-- >>> runParserTest optFormat ["--format", "yaml"]
-- Success OutputFormatYaml
optFormat :: OA.Parser OutputFormat
optFormat =
  OA.option (OA.eitherReader parseFormat) $
    OA.short 'f'
      <> OA.long "format"
      <> OA.metavar "FORMAT"
      <> OA.value OutputFormatText
      <> OA.showDefaultWith formatToString
      <> OA.help [i|Output format: #{available}|]
  where
    allFormats = fmap formatToString ([minBound .. maxBound] :: [OutputFormat])
    available = Data.List.intercalate ", " (Data.List.init allFormats) <> " or " <> Data.List.last allFormats


-- *** Version


-- | CLI option parser for application version.
optVersion :: OA.Parser (a -> a)
optVersion =
  OA.infoOption versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- ** Subcommand Helpers


-- | Shared program header.
header :: OA.InfoMod a
header = OA.header ("hebele - Hebele v" <> versionString)


-- | Convenience function for creating new subcommands.
mkSubCommand
  :: String
  -- ^ Subcommand.
  -> OA.Parser a
  -- ^ Parser.
  -> String
  -- ^ Program description.
  -> String
  -- ^ Further description in footer.
  -> OA.Parser a
mkSubCommand c x d f = OA.hsubparser (OA.command c (OA.info x infomod) <> OA.metavar c)
  where
    infomod = OA.fullDesc <> header <> OA.progDesc d <> OA.footer f


-- | Convenience function for creating new subcommands.
mkSubCommandDoc
  :: String
  -- ^ Subcommand.
  -> OA.Parser a
  -- ^ Parser.
  -> String
  -- ^ Program description.
  -> OA.Help.Doc
  -- ^ Further description in footer in Doc form.
  -> OA.Parser a
mkSubCommandDoc c x d f = OA.hsubparser (OA.command c (OA.info x infomod) <> OA.metavar c)
  where
    infomod = OA.fullDesc <> header <> OA.progDesc d <> OA.footerDoc (Just f)


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
