{
  description = "";

  inputs = {
    getnix.url = "github:exclusive-and/get.nix";
  };

  outputs = {
    self
  , getnix
  , nixpkgs
  }:
  let
    systems = [
      "x86_64-linux"
    ];

    forAllSystems = getnix.lib.genAttrs systems;
  in
  {
    packages = forAllSystems (system: {
      default = import ./. {
        getnix = getnix.getnix.${system};
      };
    });

    devShells = forAllSystems (system: {
      default = import ./. {
        getnix = getnix.getnix.${system};
        isShell = true;
      };
    });
  };
}
