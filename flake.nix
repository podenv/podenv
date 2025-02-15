# Build release with: nix -L build .#release
{
  description = "Podenv";
  nixConfig.bash-prompt = "[nix(podenv)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/d3780c92e64472e8f9aa54f7bbb0dd4483b98303";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      rev = if self ? rev then
        self.rev
      else
        throw "Refusing to build from a dirty Git tree!";

      preludeSrc = pkgs.fetchFromGitHub {
        owner = "dhall-lang";
        repo = "dhall-lang";
        rev = "v17.0.0";
        sha256 = "0jnqw50q26ksxkzs85a2svyhwd2cy858xhncq945bmirpqrhklwf";
      };

      # final build
      podenvPkg = hspkgs: commit:
        (hspkgs.callCabal2nix "podenv" self { }).overrideAttrs (_: {
          # Set build environment variable to avoid warnings
          LANG = "en_US.UTF-8";
          XDG_CACHE_HOME = "/tmp";
          # Provide a local dhall prelude because build can't access network
          DHALL_PRELUDE = "${preludeSrc}/Prelude/package.dhall";
          PODENV_COMMIT = commit;
        });

      basePkg = podenvPkg pkgs.haskellPackages;

      # A package set for building haskell package with musl
      # As mentioned in https://github.com/cdepillabout/example-static-haskell-nix/blob/master/nix/overlay.nix#L11
      # This needs to be ghc-9.4 or earlier
      staticSet = pkgs.pkgsMusl.haskell.packages.ghc927.extend
        (hpFinal: hpPrev: {
          # quickcheck internal test takes forever
          QuickCheck = pkgs.haskell.lib.dontCheck hpPrev.QuickCheck;
        });

      # Make a static executable, borrowed from dhall-haskell
      statify = drv:
        pkgs.haskell.lib.appendConfigureFlags
        (pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskell.lib.dontCheck drv)) [
            "--enable-executable-static"
            "--extra-lib-dirs=${
              pkgs.pkgsMusl.ncurses.override { enableStatic = true; }
            }/lib"
            "--extra-lib-dirs=${
              pkgs.pkgsMusl.gmp6.override { withStatic = true; }
            }/lib"
            "--extra-lib-dirs=${pkgs.pkgsMusl.zlib.static}/lib"
            "--extra-lib-dirs=${
              pkgs.pkgsMusl.libsodium.overrideAttrs
              (old: { dontDisableStatic = true; })
            }/lib"
            "--extra-lib-dirs=${
              pkgs.pkgsMusl.libffi.overrideAttrs
              (old: { dontDisableStatic = true; })
            }/lib"
          ];

      exe = pkgs.haskell.lib.justStaticExecutables (basePkg rev);
      static-exe = statify (podenvPkg staticSet rev);
      pkg = basePkg "";

      release = pkgs.runCommand "podenv-release" { } ''
        echo Creating release tarball with ${static-exe}
        cd ${static-exe};
        tar --owner=0 --group=0 --mode='0755' -cf - bin/ | ${pkgs.bzip2}/bin/bzip2 -9 > $out
        echo cp $out podenv-x86_64-linux.tar.bz2
      '';

      weeder_wrapper = pkgs.writeScriptBin "weeder" ''
        #!/bin/sh
        exec ${pkgs.haskellPackages.weeder}/bin/weeder --require-hs-files --config ./.weeder.toml
      '';

    in {
      packages."x86_64-linux".default = pkgs.haskell.lib.dontCheck exe;
      apps."x86_64-linux".default = {
        type = "app";
        program = "${exe}/bin/podenv";
      };
      packages."x86_64-linux".static = static-exe;
      packages."x86_64-linux".release = release;
      devShells."x86_64-linux".hoogle = pkgs.haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          (pkgs.writeScriptBin "run" ''
            exec hoogle server -p 8080 --local --haskell
          '')

        ];
        withHoogle = true;
      };

      devShells."x86_64-linux".ci = pkgs.haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          pkgs.cabal-install
          pkgs.hlint
          pkgs.haskellPackages.fourmolu
          pkgs.haskellPackages.doctest
          weeder_wrapper
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
            fourmolu -i src/ app/ test/

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
      devShell."x86_64-linux" = pkgs.haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          (pkgs.writeScriptBin "ghcid" ''
            #!/bin/sh
            exec ${pkgs.ghcid}/bin/ghcid --command='cabal v2-repl $*'
          '')
          (pkgs.writeScriptBin "ghcid-test" ''
            #!/bin/sh
            exec ${pkgs.ghcid}/bin/ghcid --command='cabal v2-repl test:tests' -W --test Main.main
          '')
          pkgs.cabal-install
          pkgs.haskellPackages.hlint
          pkgs.haskellPackages.fourmolu
          pkgs.haskell-language-server
          weeder_wrapper
        ];
      };
    };
}
