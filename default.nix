{
  sources ? import ./nix/sources.nix
, ...
}@args:

let

  defaultArgs = {
    lib = import "${sources.nixpkgs}/lib";
    nixpkgs = import sources.nixpkgs {};
  };

  project = {
    name = "hummingbird";
    src = ./code;
    version = "0";
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
