{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
  let
    inherit (inputs.nixpkgs) lib;

    forEachSystem = systems: f:
      lib.foldl' lib.recursiveUpdate {} (lib.map f systems);
  in
    forEachSystem ["x86_64-linux"] (
      system:
      let
        ghcVersion = "ghc912";
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        project = pkgs.callPackage ./nix/package.nix {
          haskellPackages = pkgs.haskell.packages."${ghcVersion}";
        };
      in
      {
        packages.${system}.default = project;
        devShells.${system}.default = project.devShell;
      }
    );
}
