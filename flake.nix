{
  description = "Tenpureto";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # This is a workaround to some flakes problem
    nixpkgs-unstable.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, haskellNix }:
    let

      modules = [
        # { doHaddock = false; }
        ({ pkgs, ... }: { packages.cryptonite.flags.integer-gmp = false; })
        # https://github.com/input-output-hk/haskell.nix/issues/1177
        ({ lib, ... }: {
          options.nonReinstallablePkgs =
            lib.mkOption { apply = x: [ "exceptions" "stm" ] ++ x; };
        })
      ];

      project = system:
        let pkgs = haskellNix.legacyPackages.${system};
        in pkgs.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "tenpureto";
            src = ./.;
          };
          inherit modules;
        };

      drv = system: (project system).tenpureto.components.exes.tenpureto;

      staticProject = # #
        let pkgs = haskellNix.legacyPackages.x86_64-linux;
        in pkgs.pkgsCross.musl64.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "tenpureto";
            src = ./.;
          };
          modules = modules ++ [{
            packages.tenpureto.components.exes.tenpureto.configureFlags = [
              "--disable-executable-dynamic"
              "--disable-shared"
              "--ghc-option=-optl=-pthread"
              "--ghc-option=-optl=-static"
            ];
          }];
        };

      tenpureto-static = staticProject.tenpureto.components.exes.tenpureto;

      tenpureto-package = fmt:
        with import nixpkgs { system = "x86_64-linux"; };
        stdenv.mkDerivation {
          name = "${tenpureto-static.package.identifier.name}-${fmt}";
          version = tenpureto-static.package.identifier.version;
          src = self;
          buildInputs = [ fpm rpm ];
          buildPhase = ''
            mkdir -p bash_completions zsh_completions $out &&
            cp ${tenpureto-static}/bin/tenpureto ./ &&
            ${tenpureto-static}/bin/tenpureto --bash-completion-script /usr/bin/tenpureto >bash_completions/tenpureto &&
            ${tenpureto-static}/bin/tenpureto --zsh-completion-script /usr/bin/tenpureto >zsh_completions/_tenpureto &&
            fpm --input-type dir \
                --output-type ${fmt} \
                --force \
                --package $out/ \
                --name "${tenpureto-static.package.identifier.name}" \
                --version "${tenpureto-static.package.identifier.version}" \
                --url "${tenpureto-static.meta.homepage}" \
                --description "${tenpureto-static.meta.description}" \
                --maintainer "Roman Timushev" \
                --depends git \
                --deb-no-default-config-files \
                tenpureto=/usr/bin/ \
                ./bash_completions/tenpureto=/etc/bash_completion.d/ \
                ./zsh_completions/_tenpureto=/usr/share/zsh/site-functions/
          '';
          dontInstall = true;
        };

      tenpureto-dist = with import nixpkgs { system = "x86_64-linux"; };
        symlinkJoin {
          name = "${tenpureto-static.package.identifier.name}-dist";
          version = tenpureto-static.package.identifier.version;
          paths = [
            tenpureto-static
            (tenpureto-package "deb")
            (tenpureto-package "rpm")
          ];
        };

      shell = system:
        (project system).shellFor {
          exactDeps = true;
          withHoogle = true;
          tools = {
            brittany = { inherit modules; };
            haskell-language-server = {
              modules = modules ++ [{
                packages.haskell-language-server.flags = {
                  ormolu = false;
                  fourmolu = false;
                  floskell = false;
                  stylishhaskell = false;
                };
              }];
            };
          };
        };

    in {
      packages.x86_64-darwin = { # #
        tenpureto = drv "x86_64-darwin";
      };
      packages.aarch64-darwin = { # #
        tenpureto = drv "aarch64-darwin";
      };
      packages.x86_64-linux = { # #
        tenpureto = drv "x86_64-linux";
        inherit tenpureto-static tenpureto-dist;
      };
      packages.aarch64-linux = { # #
        tenpureto = drv "aarch64-linux";
        inherit tenpureto-static tenpureto-dist;
      };

      defaultPackage.x86_64-darwin = self.packages.x86_64-darwin.tenpureto;
      defaultPackage.aarch64-darwin = self.packages.x86_64-darwin.tenpureto;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.tenpureto;
      defaultPackage.aarch64-linux = self.packages.aarch64-linux.tenpureto;

      apps.x86_64-darwin = {
        tenpureto = {
          type = "app";
          program = "${self.defaultPackage.x86_64-darwin}/bin/tenpureto";
        };
      };

      apps.aarch64-darwin = {
        tenpureto = {
          type = "app";
          program = "${self.defaultPackage.aarch64-darwin}/bin/tenpureto";
        };
      };

      apps.x86_64-linux = {
        tenpureto = {
          type = "app";
          program = "${self.defaultPackage.x86_64-linux}/bin/tenpureto";
        };
      };

      apps.aarch64-linux = {
        tenpureto = {
          type = "app";
          program = "${self.defaultPackage.aarch64-linux}/bin/tenpureto";
        };
      };

      defaultApp.x86_64-darwin = self.apps.x86_64-darwin.tenpureto;
      defaultApp.aarch64-darwin = self.apps.aarch64-darwin.tenpureto;
      defaultApp.x86_64-linux = self.apps.x86_64-linux.tenpureto;
      defaultApp.aarch64-linux = self.apps.aarch64-linux.tenpureto;

      devShell.x86_64-darwin = shell "x86_64-darwin";
      devShell.aarch64-darwin = shell "aarch64-darwin";
      devShell.x86_64-linux = shell "x86_64-linux";
      devShell.aarch64-linux = shell "aarch64-linux";
    };

}
