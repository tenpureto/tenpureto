let
  haskellNix = import (builtins.fetchTarball
    "https://github.com/input-output-hk/haskell.nix/archive/788e1983768cd810d94e77f4032288fb26e486d4.tar.gz")
    { };
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in { pkgs ? import nixpkgsSrc nixpkgsArgs }:
let
  modules = haskellPackages: [
    { reinstallableLibGhc = true; }
    { packages.ghc.patches = [ ./ghc.patch ]; }
    { packages.terminal-size.patches = [ ./terminal-size.patch ]; }
    {
      packages.tenpureto.components.tests.tenpureto-test.build-tools =
        [ haskellPackages.tasty-discover ];
    }
    {
      packages.tenpureto.components.tests.tenpureto-test.testWrapper =
        [ "echo" ];
    }
  ];
  staticCrossPkgs =
    if pkgs.stdenv.hostPlatform.isLinux then pkgs.pkgsCross.musl64 else pkgs;
in {
  default = with pkgs;
    haskell-nix.stackProject {
      src = haskell-nix.haskellLib.cleanGit {
        src = ./.;
        name = "tenpureto";
      };
      modules = modules haskell-nix.haskellPackages;
    };
  static = with staticCrossPkgs;
    let
      libffi-static = libffi.overrideAttrs (oldAttrs: {
        dontDisableStatic = true;
        configureFlags = (oldAttrs.configureFlags or [ ])
          ++ [ "--enable-static" "--disable-shared" ];
      });
    in haskell-nix.stackProject {
      src = haskell-nix.haskellLib.cleanGit {
        src = ./.;
        name = "tenpureto";
      };
      modules = (modules haskell-nix.haskellPackages) ++ [
        { doHaddock = false; }
        {
          ghc.package =
            buildPackages.pkgs.haskell-nix.compiler.ghc883.override {
              enableIntegerSimple = true;
              enableShared = true;
            };
        }
        { packages.ghc.flags.terminfo = false; }
        { packages.bytestring.flags.integer-simple = true; }
        { packages.text.flags.integer-simple = true; }
        { packages.scientific.flags.integer-simple = true; }
        { packages.integer-logarithms.flags.integer-gmp = false; }
        { packages.cryptonite.flags.integer-gmp = false; }
        { packages.hashable.flags.integer-gmp = false; }
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
