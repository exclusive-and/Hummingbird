# When in doubt, I bootstrap myself with the nixpkgs from sources.nix
{
  localSystem ? builtins.currentSystem
, crossSystem ? localSystem
, sources ? import ./sources.nix
}:
import sources.nixpkgs {
  config = {};
  inherit localSystem crossSystem;
}
