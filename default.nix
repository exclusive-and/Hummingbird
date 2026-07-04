{
  ghcVersion ? "ghc912"
, pkgs ? import ./nix/nixpkgs.nix {}
, ...
}@args:

pkgs.callPackage ./nix/package.nix {
  haskellPackages = pkgs.haskell.packages."${ghcVersion}";
}
