{
  ghcVersion ? "ghc912"
, pkgs ? import ./nix/nixpkgs.nix {}
, ...
}@args:

pkgs.callPackage ./package.nix {
  haskellPackages = pkgs.haskell.packages."${ghcVersion}";
}
