# SPDX-FileCopyrightText: 2021 Arthur Breitman
#
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

let
  sources = import ./sources.nix;
  haskellNix = import sources."haskell.nix" {};
  nixpkgs = import sources.nixpkgs;

  # override hackage and stackage index pins to update them independently of haskell.nix
  pinHackageOverlay = self: super: {
    haskell-nix = super.haskell-nix // {
      hackageSrc = sources."hackage.nix";
      stackageSrc = sources."stackage.nix";
    };
  };

  nixpkgsArgs = haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ pinHackageOverlay ];
  };

in nixpkgs nixpkgsArgs
