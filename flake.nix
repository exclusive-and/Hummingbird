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
        project = import ./. {
          inherit lib;
          nixpkgs = inputs.nixpkgs.legacyPackages.${system};
        };
      in
      {
        packages.${system}.default = project;
        devShells.${system}.default = project.devShell;
      }
    );
}
