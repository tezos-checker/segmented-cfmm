-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# LANGUAGE ApplicativeDo #-}
module Main
  ( main
  ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePretty)
import Fmt (pretty)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import Paths_segmented_cfmm (version)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Util.CLI
import Util.Main
import Util.Named

import SegCFMM.TZIP16Metadata
import SegCFMM.Types
import Typescript

main :: IO ()
main = wrapMain $ do
  cmdLnArgs <- Opt.execParser programInfo
  case cmdLnArgs of
    PrintMetadata mc ->
      putTextLn . decodeUtf8 . encodePretty $
        mkSegCfmmMetadata (mkMetadataSettings mc)
    GenerateTypescript fp ->
      void $ generateTs @Parameter fp

--------------------------------------------------------------------------------
-- Arguments parsing
--------------------------------------------------------------------------------

data CmdArgs
  = PrintMetadata MetadataConfig
  | GenerateTypescript FilePath

cmdArgsParser :: Opt.Parser CmdArgs
cmdArgsParser = asum
  [ Opt.hsubparser $
      mkCommandParser "print-metadata"
        (PrintMetadata <$> metadataConfigParser)
        "Print the TZIP-16 metadata."
  , Opt.hsubparser $
      mkCommandParser "generate-typescript"
        (GenerateTypescript <$> (mkCLOptionParser Nothing (#name  .! "target") (#help .! "Path to which generated files should be written.")))
        "Generate typescript type to represent the parameter"
  ]

programInfo :: Opt.ParserInfo CmdArgs
programInfo = Opt.info (Opt.helper <*> versionOption <*> cmdArgsParser) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "segmented-cfmm: CLI tool for Segmented CFMM contract."
  ]
  where
    versionOption = Opt.infoOption ("segmented-cfmm-" <> showVersion version)
      (Opt.long "version" <> Opt.help "Show version.")

-- | Parse metadata for token with given name and given default values.
tokenMetadataParser
  :: String
  -> Text
  -> Text
  -> Word16
  -> Opt.Parser FA2.TokenMetadata
tokenMetadataParser prefix defSymbol defName defDecimals = do
  symbol <-
    mkCLOptionParser (Just defSymbol) (#name .! (prefix <> "-token-symbol"))
    (#help .! "Symbol of the token (according to TZIP-12)")
  name <-
    mkCLOptionParser (Just defName) (#name .! (prefix <> "-token-name"))
    (#help .! "Name of the token (according to TZIP-12)")
  decimals <-
    mkCLOptionParser (Just defDecimals) (#name .! (prefix <> "-token-decimals"))
    (#help .! "Decimals field of the token (according to TZIP-12)")
  return $ FA2.mkTokenMetadata symbol name (pretty decimals)

metadataConfigParser :: Opt.Parser MetadataConfig
metadataConfigParser = do
  mcTokenXMetadata <-
    tokenMetadataParser "x" "token_x" "Token X" 1
  mcTokenYMetadata <-
    tokenMetadataParser "y" "token_y" "Token Y" 2
  return MetadataConfig{..}
