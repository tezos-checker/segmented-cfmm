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

  # all local packages and their subdirectories
  # we need to know subdirectories to make weeder stuff work
  local-packages = [
    { name = "segmented-cfmm"; subdirectory = "./haskell"; }
  ];

  # names of all local packages
  local-packages-names = map (p: p.name) local-packages;

  # haskell.nix package set
  # parameters:
  # - release – 'true' for "release" (e. g. master) build,
  #   'false' for "development" (e. g. PR) build.
  # - commitSha, commitDate – git revision info used for contract documentation.
  hs-pkgs = { release, commitSha ? null, commitDate ? null }: pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };

    modules = [
      {
        # common options for all local packages:
        packages = pkgs.lib.genAttrs local-packages-names (packageName: {
          ghcOptions = with pkgs.lib; (
            ["-O0" "-Werror"]
            # produce *.dump-hi files, required for weeder:
            ++ optionals (!release) ["-ddump-to-file" "-ddump-hi"]
          );

          # enable haddock for local packages
          doHaddock = true;

          # in non-release mode collect all *.dump-hi files (required for weeder)
          postInstall = if release then null else weeder-hacks.collect-dump-hi-files;
        });

        # disable haddock for dependencies
        doHaddock = false;
      }

      # provide commit sha and date for release mode:
      {
        packages.segmented-cfmm = {
          preBuild = ''
            mkdir -p ./test
            cp -r ${build-ligo}/* ./test
          '';
        };
      }
    ];
  };

  hs-pkgs-development = hs-pkgs { release = false; };

  # component set for all local packages like this:
  # { segmented-cfmm = { library = ...; exes = {...}; tests = {...}; ... };
  #   ...
  # }
  packages = pkgs.lib.genAttrs local-packages-names (packageName: hs-pkgs-development."${packageName}".components);

  # returns a list of all components (library + exes + tests + benchmarks) for a package
  get-package-components = pkg: with pkgs.lib;
    optional (pkg ? library) pkg.library
    ++ attrValues pkg.exes
    ++ attrValues pkg.tests
    ++ attrValues pkg.benchmarks;

  # per-package list of components
  components = pkgs.lib.mapAttrs (pkgName: pkg: get-package-components pkg) packages;

  # a list of all components from all packages in the project
  all-components = with pkgs.lib; flatten (attrValues components);

  # build haddock
  haddock = with pkgs.lib; flatten (attrValues
    (mapAttrs (pkgName: pkg: optional (pkg ? library) pkg.library.haddock) packages));

  build-ligo = pkgs.stdenv.mkDerivation {
    name = "segmented_cfmm";
    src = ./.;
    nativeBuildInputs = [ ligo ];
    buildPhase = "make all";
    installPhase = "mkdir -p $out; cp -r out/* $out";
  };

  build-typescript = buildYarnPackage {
    src = ./typescript/segmented-cfmm;
    yarnBuild = ''
      yarn install
      yarn tsc
    '';
  };

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  # a derivation which generates a script for running weeder
  weeder-script = weeder-hacks.weeder-script {
    weeder = weeder-legacy;
    hs-pkgs = hs-pkgs-development;
    local-packages = local-packages;
  };

  # stack2cabal in nixpkgs is broken because fixes for ghc 8.10 have not
  # been released to hackage yet, take sources from github
  stack2cabal = pkgs.haskellPackages.stack2cabal.overrideAttrs (o: {
    src = pkgs.fetchFromGitHub {
      owner = "hasufell";
      repo = "stack2cabal";
      rev = "afa113beb77569ff21f03fade6ce39edc109598d";
      sha256 = "1zwg1xkqxn5b9mmqafg87rmgln47zsmpgdkly165xdzg38smhmng";
    };
  });

}
