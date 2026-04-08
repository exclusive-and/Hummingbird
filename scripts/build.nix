args0@{
  lib

  # The version of GHC that will be used for the build.
, ghcVersion ? "ghc912"

, nixpkgs

, haskellPackages ?
    nixpkgs.haskell.packages.${ghcVersion}

, ...
}:
{
  name
, version
, src
, depends ? _: []
}:
let
  depsSystem = [
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
  ];

  depsTools = [
    haskellPackages.cabal-install
  ];

  drv = haskellPackages.mkDerivation {
    pname = name;
    inherit src version;

    libraryHaskellDepends = depends haskellPackages;
    librarySystemDepends = depsSystem;

    passthru = {
      inherit ghcVersion haskellPackages;

      devShell = haskellPackages.shellFor {
        packages = _: [ drv ];
        buildInputs = depsSystem;
        nativeBuildInputs = depsTools;
      };
    };
  };
in
  drv
