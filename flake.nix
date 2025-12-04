{
  description = "";

  inputs.nixpkgs.url = "nixpkgs/nixos-25.11";

  outputs = {self, nixpkgs}:
    let
      systems = [
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      main = forAllSystems (
        system:
        import ./. {
          pkgs = nixpkgs.legacyPackages.${system};
        }
      );
    in
    {
      packages = forAllSystems (system: main.${system}.outputs);
      devShells = forAllSystems (
        system: {
          default = main.${system}.devShell {};
        }
      );
    };
}
