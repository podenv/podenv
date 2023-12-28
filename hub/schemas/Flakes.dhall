-- | A podenv nix runtime description:
--
-- - `nixpkgs` is an optional pin for the main package set
--   when set, this is added to the nix command: ["--override-input" "nixpkgs" "${nixpkgs}"]
--   so that installables inputs share the same set.
--   This is necessary for nixGL, see: https://github.com/guibou/nixGL/pull/97
-- - `installables` is a list of nix flakes,
--   see: https://nixos.org/manual/nix/stable/command-ref/new-cli/nix.html#installables
--
{ Type =
    { nixpkgs : Optional Text
    , installables : List Text
    , -- | Cache can be set to a cachix public key, e.g. "podenv.cachix.org-1:FA80Dv5XSHxzMYOg1wEANhDN7h43nN8twBJiYNzcDGY="
      cache : Optional Text
    }
, default = { nixpkgs = None Text, cache = None Text }
}
