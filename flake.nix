# Build release with: nix -L build .#release
{
  description = "Podenv";
  nixConfig.bash-prompt = "[nix(podenv)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/0062866e8a9c427964d69b3d38721f6b42d10534";
      # "path:///srv/github.com/podenv/hspkgs";
  };

  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      hubCommit = "4c496f98e9c300d378ec6ca993de43a69261f95e";
      hubHash = "sha256-KJUMTZ0aWFoslzFEVXz2ftkKl33HVZnryZkzx9T2TgQ=";
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
      hubSrc = pkgs.fetchFromGitHub {
        owner = "podenv";
        repo = "hub";
        rev = hubCommit;
        sha256 = hubHash;
      };
      podenvSrc = pkgs.runCommand "make-podenv-src" { } ''
        mkdir $out
        ${pkgs.rsync}/bin/rsync --exclude hub -r ${self}/ $out/
        echo "Building $out"
        ln -sf ${hubSrc} $out/hub
      '';

      podenvPkg = hspkgs: commit:
        (hspkgs.callCabal2nix "podenv" podenvSrc { }).overrideAttrs (_: {
          # Set build environment variable to avoid warnings
          LANG = "en_US.UTF-8";
          XDG_CACHE_HOME = "/tmp";
          # Provide a local dhall prelude because build can't access network
          DHALL_PRELUDE = "${preludeSrc}/Prelude/package.dhall";
          HUB_COMMIT = hubCommit;
          PODENV_COMMIT = commit;
        });

      basePkg = podenvPkg pkgs.hspkgs;
      exe = pkgs.haskell.lib.justStaticExecutables (basePkg rev);
      static-exe = hspkgs.mk-static-haskell (podenvPkg pkgs.hspkgsMusl rev);
      pkg = basePkg "";

      release = pkgs.runCommand "podenv-release" { } ''
        echo Creating release tarball with ${static-exe}
        cd ${static-exe};
        tar -cf - bin/ | ${pkgs.bzip2}/bin/bzip2 -9 > $out
        echo cp $out podenv-x86_64-linux.tar.bz2
      '';

      weeder_wrapper = pkgs.writeScriptBin "weeder" ''
        #!/bin/sh
        exec ${pkgs.weeder}/bin/weeder --require-hs-files --config ./.weeder.dhall
      '';

    in {
      packages."x86_64-linux".default = exe;
      packages."x86_64-linux".static = static-exe;
      packages."x86_64-linux".release = release;
      devShells."x86_64-linux".hoogle = pkgs.hspkgs.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          (pkgs.writeScriptBin "run" ''
            exec hoogle server -p 8080 --local --haskell
          '')

        ];
        withHoogle = true;
      };

      devShells."x86_64-linux".ci = pkgs.hspkgs.shellFor {
        packages = p: [ pkg ];
        buildInputs = [
          pkgs.cabal-install
          pkgs.hlint
          pkgs.ormolu
          pkgs.hspkgs.doctest
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
      devShell."x86_64-linux" = pkgs.hspkgs.shellFor {
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
          pkgs.hlint
          pkgs.haskell-language-server
          weeder_wrapper
        ];
      };
    };
}
