{
  system ? builtins.currentSystem
, localSystem ? system
, crossSystem ? localSystem
, sources ? import ../sources.nix
, ...
} @ args:
let
  # When in doubt, I bootstrap myself from a known copy of nixpkgs.
  bootpkgs = import sources.nixpkgs {
    config = args.config or {};
    inherit localSystem crossSystem;
  };

  pkgs = args.pkgs or bootpkgs;

  ghcVersion = args.ghcVersion or "ghc9103";
  ghcHackage = pkgs.haskell.packages.${ghcVersion};
in
{
  inherit (pkgs) lib;
  inherit ghcHackage;
  # Build the Nix derivation for a Haskell Cabal package.
  buildHs = ghcHackage.mkDerivation;
  # Cabal CLI (for dev-shell).
  cabal = ghcHackage.cabal-install;
  # Haskell language server (for dev-shell).
  hls = ghcHackage.haskell-language-server;
}
