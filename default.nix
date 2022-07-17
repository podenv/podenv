# Nix expressions to work on podenv
# Build release with: `PODENV_COMMIT=$(git show HEAD --format="format:%H" -q) nix-build --attr release
{ withHoogle ? false }:
let
  # pin the upstream nixpkgs
  nixpkgsPath = fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/d46be5b0e8baad998f8277e04370f0fd30dde11b.tar.gz";
    sha256 = "sha256:06mjr9k3hi4d9cnffxmhh48fnlsjqcav2r9zghcb62m3zcaxvsr9";
  };
  nixpkgsSrc = (import nixpkgsPath);

  # update haskell dependencies
  compilerVersion = "923";
  compiler = "ghc" + compilerVersion;

  pkgsBase = nixpkgsSrc { system = "x86_64-linux"; };

  getPkgs = static:
    let
      pkgs = (if static then pkgsBase.pkgsMusl else pkgsBase);

      # fetch the DHALL_PRELUDE to compile the podenv/hub without network access
      preludeSrc = pkgs.fetchFromGitHub {
        owner = "dhall-lang";
        repo = "dhall-lang";
        rev = "v17.0.0";
        sha256 = "0jnqw50q26ksxkzs85a2svyhwd2cy858xhncq945bmirpqrhklwf";
      };

      # use gitignore.nix to filter files from the src and avoid un-necessary rebuild
      gitignoreSrc = pkgs.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        # put the latest commit sha of gitignore Nix library here:
        rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
        # use what nix suggests in the mismatch message here:
        sha256 = "sha256-qHu3uZ/o9jBHiA3MEKHJ06k7w4heOhA+4HCSIvflRxo=";
      };
      inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

      haskellOverrides = {
        overrides = hpFinal: hpPrev: {
          relude = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.relude {
              version = "1.1.0.0";
              sha256 = "sha256-tR3wipPvEzHdVjieFY5nrHtoxizBVhwokNNXLHZKtgk=";
            });

          linux-capabilities =
            pkgs.haskell.lib.overrideCabal hpPrev.linux-capabilities {
              version = "0.1.1.0";
              sha256 = "sha256-xrLOxd8K8p+vnXJ8cmqHdgY4ZE5DNYEWnHWNp1sjuEg=";
            };

          podenv = (hpPrev.callCabal2nix "podenv" (gitignoreSource ./.)
            { }).overrideAttrs (_: {
              # Set build environment variable to avoid warnings
              LANG = "en_US.UTF-8";
              XDG_CACHE_HOME = "/tmp";
              # Provide a local dhall prelude because build can't access network
              DHALL_PRELUDE = "${preludeSrc}/Prelude/package.dhall";
              HUB_COMMIT = "${builtins.readFile ./.git/modules/hub/HEAD}";
              PODENV_COMMIT = builtins.getEnv "PODENV_COMMIT";
            });
        };
      };

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
                  pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })
                }/lib"
              ];

      hsPkgs = pkgs.haskell.packages.${compiler}.override haskellOverrides;

    in {
      hsPkgs = hsPkgs;
      podenv-static = statify hsPkgs.podenv;
    };

  hsPkgs = (getPkgs false).hsPkgs;
  podenv-static = (getPkgs true).podenv-static;

in {
  podenv = hsPkgs.podenv;
  static = podenv-static;

  release = pkgsBase.runCommand "podenv-release" { } ''
    echo Creating release tarball with ${podenv-static}
    cd ${podenv-static};
    tar -cf - bin/ | ${pkgsBase.bzip2}/bin/bzip2 -9 > $out
    echo cp $out podenv-x86_64-linux.tar.bz2
  '';

  shell = hsPkgs.shellFor {
    packages = p: [ p.podenv ];
    buildInputs = with hsPkgs; [ cabal-install hlint ghcid doctest ];
    withHoogle = withHoogle;
  };
}
