let
  pkgs = import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/98ac006e995d310c9329787bd93f6c3384174713.tar.gz));
  modules = haskellPackages: [
    { reinstallableLibGhc = true; }
    { packages.ghc.patches = [ ./ghc-8.6.5.patch ]; }
    { packages.terminal-size.patches = [ ./terminal-size.patch ]; }
    { packages.tenpureto.components.tests.tenpureto-test.build-tools = [ haskellPackages.tasty-discover ]; }
    { packages.tenpureto.components.tests.tenpureto-test.testWrapper = [ "echo" ]; }
  ];
  staticCrossPkgs = if pkgs.stdenv.hostPlatform.isLinux then pkgs.pkgsCross.musl64 else pkgs;
in {
  default = with pkgs; haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = modules haskell-nix.haskellPackages;
  };
  static = with staticCrossPkgs; let
    libffi-static = libffi.overrideAttrs (oldAttrs: {
      dontDisableStatic = true;
      configureFlags = (oldAttrs.configureFlags or []) ++ [
        "--enable-static"
        "--disable-shared"
      ];
    });
  in haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = (modules haskell-nix.haskellPackages) ++ [
      { doHaddock = false; }
      {
        ghc.package = buildPackages.pkgs.haskell-nix.compiler.ghc865.override {
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
  # debug
  inherit pkgs;
}
