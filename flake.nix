{
  description = "Tenpureto";

  inputs = { haskell.url = "github:input-output-hk/haskell.nix"; };

  outputs = { self, haskell }:
    let

      project = system:
        let pkgs = haskell.legacyPackages.${system};
        in pkgs.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "tenpureto";
            src = ./.;
          };
        };

      drv = system: (project system).tenpureto.components.exes.tenpureto;

      staticProject = system:
        let pkgs = haskell.legacyPackages.${system};
        in pkgs.pkgsCross.musl64.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "tenpureto";
            src = ./.;
          };
          modules = [
            { doHaddock = false; }
            ({ pkgs, ... }: {
              ghc.package =
                pkgs.buildPackages.haskell-nix.compiler.ghc883.override {
                  enableIntegerSimple = true;
                }
              else
                pkgs.buildPackages.haskell-nix.compiler.ghc884;
              packages.bytestring.flags.integer-simple = true;
              packages.text.flags.integer-simple = true;
              packages.scientific.flags.integer-simple = true;
              packages.integer-logarithms.flags.integer-gmp = false;
              packages.cryptonite.flags.integer-gmp = false;
              packages.hashable.flags.integer-gmp = false;
            })
            {
              packages.tenpureto.components.exes.tenpureto.configureFlags = [
                "--disable-executable-dynamic"
                "--disable-shared"
                "--ghc-option=-optl=-pthread"
                "--ghc-option=-optl=-static"
              ];
            }
          ];
        };

      staticDrv = system:
        (staticProject system).tenpureto.components.exes.tenpureto;

      shell = system:
        (project system).shellFor {
          exactDeps = true;
          withHoogle = true;
          #tools = { brittany = "0.13.1.0"; };
        };

    in {
      packages.x86_64-darwin = { # #
        tenpureto = drv "x86_64-darwin";
      };
      packages.x86_64-linux = { # #
        tenpureto = drv "x86_64-linux";
        tenpureto-static = staticDrv "x86_64-linux";
      };

      defaultPackage.x86_64-darwin = self.packages.x86_64-darwin.tenpureto;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.tenpureto;

      apps.x86_64-darwin = {
        tenpureto = {
          type = "app";
          program = "${self.defaultPackage.x86_64-darwin}/bin/tenpureto";
        };
      };

      apps.x86_64-linux = {
        tenpureto = {
          type = "app";
          program = "${self.defaultPackage.x86_64-linux}/bin/tenpureto";
        };
      };

      defaultApp.x86_64-darwin = self.apps.x86_64-darwin.tenpureto;
      defaultApp.x86_64-linux = self.apps.x86_64-linux.tenpureto;

      devShell.x86_64-darwin = shell "x86_64-darwin";
      devShell.x86_64-linux = shell "x86_64-linux";
    };

}
