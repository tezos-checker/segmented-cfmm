-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | TZIP-16 metadata for SegCFMM
module SegCFMM.TZIP16Metadata
  ( MetadataConfig (..)
  , MetadataSettings (..)
  , defaultMetadataConfig
  , mkSegCfmmMetadata
  , mkMetadataSettings

  , getTokenXAddressView
  , getTokenYAddressView
  , segCfmmViews
  ) where

import Universum

import qualified Data.Map as M
import Data.Version (showVersion)

import Lorentz hiding (View)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Metadata

import qualified Paths_segmented_cfmm as Paths
import SegCFMM.Types

-- | Piece of metadata defined by user.
data MetadataConfig = MetadataConfig
  { mcTokenXMetadata :: FA2.TokenMetadata
  , mcTokenYMetadata :: FA2.TokenMetadata
  }

-- | All the information for instantiating metadata.
--
-- This includes pieces defined by user (if any) like some constants for
-- off-chain views, as well as abstract storage accessors of the contract.
data MetadataSettings = MetadataSettings
  { msConfig :: MetadataConfig
  }

-- | Default values for config.
defaultMetadataConfig :: MetadataConfig
defaultMetadataConfig =
  MetadataConfig
    { mcTokenXMetadata = FA2.mkTokenMetadata "token_x" "Token X" "1"
    , mcTokenYMetadata = FA2.mkTokenMetadata "token_y" "Token Y" "2"
    }

-- | Construct MetadataSetting for the contract.
mkMetadataSettings :: MetadataConfig -> MetadataSettings
mkMetadataSettings msConfig = MetadataSettings
  { msConfig
  }

-- | Parts of metadata that can be filled just knowing the contract,
-- without user's input.
mkSegCfmmMetadata :: MetadataSettings -> Metadata (ToT Storage)
mkSegCfmmMetadata settings =
  let
    tokenXName = M.lookup ([mt|name|]) (settings & msConfig & mcTokenXMetadata)
      & fromMaybe (error "Token X not found.")
    tokenYName = M.lookup ([mt|name|]) (settings & msConfig & mcTokenYMetadata)
      & fromMaybe (error "Token Y not found.")

    defaultDescription =
      "A constant product market making smart-contract, which allows the curve to be defined on price\
      \ segments. Based on the ideas described in https://uniswap.org/whitepaper-v3.pdf."

  in
    mconcat
      [ name ("Segmented CFMM Contract (" <> decodeUtf8 tokenXName
          <> " / " <> decodeUtf8 tokenYName <> ")")
      , description defaultDescription
      , version . fromString $ showVersion Paths.version
      , license $ License "MIT" Nothing
      , authors [Author "Serokell", Author "Arthur Breitman"]
      , homepage "https://github.com/serokell/segmented-cfmm"
      , interfaces [tzip 16]
      , views (segCfmmViews settings)
      ]

------------------------------------------------------------------------
-- Off-chain views
------------------------------------------------------------------------

segCfmmViews :: MetadataSettings -> [View $ ToT Storage]
segCfmmViews = sequence
  [ getLiquidityView
  , getSqrtPriceView
  , getTokenXAddressView
  , getTokenYAddressView
  ]

type ContractView = MetadataSettings -> View (ToT Storage)

-- SegCFMM-specific
------------------------------------------------------------------------

getLiquidityView :: ContractView
getLiquidityView MetadataSettings{} = View
  { vName = "get_liquidity"
  , vDescription = Just
      "Get the liquidity of a token"
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $
            unsafeCompileViewCode $ WithoutParam $ do
              stToField #sLiquidity
      ]
  }

getSqrtPriceView :: ContractView
getSqrtPriceView MetadataSettings{} = View
  { vName = "get_sqrt_price"
  , vDescription = Just
      "Get the square root of virtual price"
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @(X 80 Natural) Nothing [] $
            unsafeCompileViewCode $ WithoutParam $ do
              stToField #sSqrtPrice
      ]
  }

getTokenXAddressView :: ContractView
getTokenXAddressView MetadataSettings{} = View
  { vName = "get_token_x_address"
  , vDescription = Just
      "Get the address of token X"
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Address Nothing [] $
            unsafeCompileViewCode $ WithoutParam $
              stToField #sConstants #
              toField #cXTokenAddress
      ]
  }

getTokenYAddressView :: ContractView
getTokenYAddressView MetadataSettings{} = View
  { vName = "get_token_y_address"
  , vDescription = Just
      "Get the address of token Y"
  , vPure = Just True
  , vImplementations =
      [ VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Address Nothing [] $
            unsafeCompileViewCode $ WithoutParam $
              stToField #sConstants #
              toField #cYTokenAddress
      ]
  }
