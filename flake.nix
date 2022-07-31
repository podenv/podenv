# Build release with: nix -L build .#release
{
  description = "Podenv";
  nixConfig.bash-prompt = "[nix(podenv)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/d46be5b0e8baad998f8277e04370f0fd30dde11b";
  };

  outputs = { self, nixpkgs }:
    let
      hubCommit = "c9329f6144da3c156138b2b86c8bde36a348a86c";
      base_pkgs = import nixpkgs { system = "x86_64-linux"; };
      rev = if self ? rev then
        self.rev
      else
        throw "Refusing to build from a dirty Git tree!";

      getPodenv = static:
        let
          pkgs = if static then base_pkgs.pkgsMusl else base_pkgs;

          preludeSrc = pkgs.fetchFromGitHub {
            owner = "dhall-lang";
            repo = "dhall-lang";
            rev = "v17.0.0";
            sha256 = "0jnqw50q26ksxkzs85a2svyhwd2cy858xhncq945bmirpqrhklwf";
          };

          haskellOverrides = {
            overrides = hpFinal: hpPrev: {
              relude = pkgs.haskell.lib.dontCheck
                (pkgs.haskell.lib.overrideCabal hpPrev.relude {
                  version = "1.1.0.0";
                  sha256 =
                    "sha256-tR3wipPvEzHdVjieFY5nrHtoxizBVhwokNNXLHZKtgk=";
                });

              linux-capabilities =
                pkgs.haskell.lib.overrideCabal hpPrev.linux-capabilities {
                  version = "0.1.1.0";
                  sha256 =
                    "sha256-xrLOxd8K8p+vnXJ8cmqHdgY4ZE5DNYEWnHWNp1sjuEg=";
                };

              podenv = hpPrev.callCabal2nix "podenv" self { };

              # pull ghc-9.2 support for weeder (https://github.com/ocharles/weeder/pull/94)
              weeder = pkgs.haskell.lib.justStaticExecutables
                (hpPrev.callCabal2nix "weeder" (builtins.fetchGit {
                  url = "https://github.com/ocharles/weeder";
                  ref = "master";
                  rev = "c58ed2a8c66dcf0b469f8343efb6b6f61c7c40f3";
                }) { });
            };
          };

          haskellPackages =
            pkgs.haskell.packages.ghc923.override haskellOverrides;

          # final build
          hubSrc = pkgs.fetchFromGitHub {
            owner = "podenv";
            repo = "hub";
            rev = hubCommit;
            sha256 = "sha256-DlyzoWpMyJjt8qyuCN8CsqM6/DvyAc0bfp64jcqqIOE=";
          };
          podenvSrc = pkgs.runCommand "make-podenv-src" { } ''
            mkdir $out
            ${pkgs.rsync}/bin/rsync --exclude hub -r ${self}/ $out/
            echo "Building $out"
            ln -sf ${hubSrc} $out/hub
          '';

          # Borrowed from https://github.com/dhall-lang/dhall-haskell/blob/master/nix/shared.nix
          statify = drv:
            pkgs.haskell.lib.appendConfigureFlags
            (pkgs.haskell.lib.disableLibraryProfiling
              (pkgs.haskell.lib.disableSharedExecutables
                (pkgs.haskell.lib.justStaticExecutables
                  (pkgs.haskell.lib.dontCheck drv)))) [
                    "--enable-executable-static"
                    "--extra-lib-dirs=${
                      pkgs.ncurses.override { enableStatic = true; }
                    }/lib"
                    "--extra-lib-dirs=${
                      pkgs.gmp6.override { withStatic = true; }
                    }/lib"
                    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                    "--extra-lib-dirs=${
                      pkgs.pkgsMusl.libsodium.overrideAttrs
                      (old: { dontDisableStatic = true; })
                    }/lib"
                    "--extra-lib-dirs=${
                      pkgs.libffi.overrideAttrs
                      (old: { dontDisableStatic = true; })
                    }/lib"
                  ];

          podenvPkg =
            (haskellPackages.callCabal2nix "podenv" podenvSrc { }).overrideAttrs
            (_: {
              # Set build environment variable to avoid warnings
              LANG = "en_US.UTF-8";
              XDG_CACHE_HOME = "/tmp";
              # Provide a local dhall prelude because build can't access network
              DHALL_PRELUDE = "${preludeSrc}/Prelude/package.dhall";
              HUB_COMMIT = hubCommit;
              PODENV_COMMIT = rev;
            });

          podenvExe = if static then
            statify podenvPkg
          else
            pkgs.haskell.lib.justStaticExecutables podenvPkg;

        in {
          haskellPackages = haskellPackages;
          exe = podenvExe;
          pkgs = pkgs;
          release = pkgs.runCommand "podenv-release" { } ''
            echo Creating release tarball with ${podenvExe}
            cd ${podenvExe};
            tar -cf - bin/ | ${pkgs.bzip2}/bin/bzip2 -9 > $out
            echo cp $out podenv-x86_64-linux.tar.bz2
          '';
          weeder_wrapper = pkgs.writeScriptBin "weeder" ''
            #!/bin/sh
            exec ${haskellPackages.weeder}/bin/weeder --require-hs-files --config ./.weeder.dhall
          '';
        };

      podenv-dynamic = getPodenv false;
      podenv-static = getPodenv true;
      pkgs = podenv-dynamic.pkgs;
      haskellPackages = podenv-dynamic.haskellPackages;
      pkg = haskellPackages.podenv;

    in {
      packages."x86_64-linux".default = podenv-dynamic.exe;
      packages."x86_64-linux".static = podenv-static.exe;
      packages."x86_64-linux".release = podenv-static.release;
      devShells."x86_64-linux".hoogle = haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          (pkgs.writeScriptBin "run" ''
            exec hoogle server -p 8080 --local --haskell
          '')

        ];
        withHoogle = true;
      };

      devShells."x86_64-linux".ci = haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          pkgs.cabal-install
          pkgs.hlint
          pkgs.ormolu
          haskellPackages.doctest_0_20_0
          podenv-dynamic.weeder_wrapper
          (pkgs.writeScriptBin "run" ''
            set -e
            function log { echo -e "\n\x1b[1;33m[+] $*\x1b[0m"; }
            cabal="cabal -O0 --enable-tests --ghc-options=-Wunused-packages --ghc-option=-Werror --test-show-details=direct"
            log "Building"
            $cabal build

            log "Testing"
            $cabal test
            cabal check

            log "Formatting"
            ormolu -o -XPatternSynonyms -o -XTypeApplications -o -XImportQualifiedPost -o -XQuasiQuotes --mode inplace $(find src/ app/ test/ -name "*.hs" | grep -v Prelude)

            log "Linting"
            hlint -XQuasiQuotes src/ app/ test/

            if type -p doctest; then
              log "Doctests"
              cabal -O0 repl --with-ghc=doctest
            fi

            if type -p weeder; then
              log "Haskell weeder"
              weeder
            fi

            log "Check for diff"
            if [ ! -z "$(git status --porcelain)" ]; then
              git status
              exit 1
            fi
          '')
        ];
      };
      devShell."x86_64-linux" = haskellPackages.shellFor {
        packages = p: [ p.podenv ];
        buildInputs = with haskellPackages; [
          (pkgs.writeScriptBin "ghcid" ''
            #!/bin/sh
            exec ${pkgs.ghcid}/bin/ghcid --command='cabal v2-repl $*'
          '')
          (pkgs.writeScriptBin "ghcid-test" ''
            #!/bin/sh
            exec ${pkgs.ghcid}/bin/ghcid --command='cabal v2-repl test:tests' -W --test Main.main
          '')
          cabal-install
          hlint
          podenv-dynamic.weeder_wrapper
          pkgs.haskell-language-server
        ];
      };
    };
}
