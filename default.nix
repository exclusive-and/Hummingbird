args@{
  sources ? import ./nix/sources.nix
, ...
}:
let
  defaultArgs = {
    lib = import "${sources.nixpkgs}/lib";
    nixpkgs = import sources.nixpkgs {};
  };

  project = {
    name = "hummingbird";
    version = "0";

    src = ./code;

    depends = haskellPackages: with haskellPackages; [
      ansi-terminal
      base
      binary
      brick
      bytestring
      containers
      crypton
      filepath
      exceptions_0_10_12
      hashable
      haskeline
      monad-chronicle
      mtl
      parsec
      prettyprinter
      prettyprinter-ansi-terminal
      shellmet
      template-haskell
      text
      text-rope
      these
      transformers
      unordered-containers
      vty
    ];
  };
in
  import ./scripts/build.nix (defaultArgs // args) project
