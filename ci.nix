# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

rec {
  sources = import ./nix/sources.nix;
  xrefcheck = import sources.xrefcheck;
  haskell-nix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  pkgs = import sources.nixpkgs haskell-nix.nixpkgsArgs;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };
  tezos-client = (import "${sources.tezos-packaging}/nix/build/pkgs.nix" {}).ocamlPackages.tezos-client;
  ligo = (pkgs.runCommand "ligo" {} "mkdir -p $out/bin; cp ${sources.ligo} $out/bin/ligo; chmod +x $out/bin/ligo");
  morley = (import "${sources.morley}/ci.nix").packages.morley.exes.morley;
  inherit (pkgs.callPackage sources.nix-npm-buildpackage { }) buildYarnPackage;

  build-ligo = pkgs.stdenv.mkDerivation {
    name = "segmented_cfmm";
    src = ./.;
    nativeBuildInputs = [ ligo ];
    buildPhase = "make all";
    installPhase = "mkdir -p $out; cp -r out/* $out";
  };

}
