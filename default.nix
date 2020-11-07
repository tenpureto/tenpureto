let
  haskellNix = import (builtins.fetchTarball
    "https://github.com/input-output-hk/haskell.nix/archive/10912efb8c9f4a09c2ec0db326248678ed0b1f62.tar.gz")
    { };
  nixpkgsSrc = haskellNix.sources.nixpkgs;
  nixpkgsArgs = haskellNix.nixpkgsArgs;

  pkgs = import nixpkgsSrc nixpkgsArgs;
  pkgsMusl = pkgs.pkgsCross.musl64;

  modules = [
    { reinstallableLibGhc = true; }
    { packages.ghc.patches = [ ./ghc.patch ]; }
    { packages.terminal-size.patches = [ ./terminal-size.patch ]; }
    ({ pkgs, ... }: {
      packages.tenpureto.components.tests.tenpureto-test.build-tools =
        [ pkgs.haskell-nix.haskellPackages.tasty-discover ];
    })
    {
      packages.tenpureto.components.tests.tenpureto-test.testWrapper =
        [ "echo" ];
    }
  ];
in {
  default = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "tenpureto";
    };
    modules = modules;
  };
  static = let
    libffi-static = pkgsMusl.libffi.overrideAttrs (oldAttrs: {
      dontDisableStatic = true;
      configureFlags = (oldAttrs.configureFlags or [ ])
        ++ [ "--enable-static" "--disable-shared" ];
    });
  in pkgsMusl.haskell-nix.stackProject {
    src = pkgsMusl.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "tenpureto";
    };
    modules = modules ++ [
      { doHaddock = false; }
      ({ pkgs, ... }:
        let isMusl = pkgs.buildPackages.targetPlatform.isMusl;
        in {
          ghc.package = if isMusl then
            pkgs.buildPackages.haskell-nix.compiler.ghc883.override {
              enableIntegerSimple = true;
              enableShared = true;
            }
          else
            pkgs.buildPackages.haskell-nix.compiler.ghc883;

          packages.bytestring.flags.integer-simple = isMusl;
          packages.text.flags.integer-simple = isMusl;
          packages.scientific.flags.integer-simple = isMusl;
          packages.integer-logarithms.flags.integer-gmp = !isMusl;
          packages.cryptonite.flags.integer-gmp = !isMusl;
          packages.hashable.flags.integer-gmp = !isMusl;
        })
      {
        packages.tenpureto.components.exes.tenpureto.configureFlags = [
          "--disable-executable-dynamic"
          "--disable-shared"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-L${libffi-static}/lib"
        ];
      }
    ];
  };
  inherit pkgs;
}
