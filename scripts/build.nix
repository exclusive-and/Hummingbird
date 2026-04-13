{
  lib
# The version of GHC that will be used for the build.
, ghcVersion ? "ghc912"
, nixpkgs
, haskellPackages ? nixpkgs.haskell.packages.${ghcVersion}
, ...
}@outerArgs:

{
  name
, pname ? name
, version
, src
, depends ? _: []
, ...
}@projectArgs:

lib.fix (
  drv:
  haskellPackages.mkDerivation (
    let
      depsHaskell = depends haskellPackages;

      depsSystem = [
        haskellPackages.cabal-install
        haskellPackages.haskell-language-server
      ];

      depsTools = [
        haskellPackages.cabal-install
      ];
    in
    {
      inherit pname version;
      inherit src;

      executableHaskellDepends = depsHaskell;
      libraryHaskellDepends = depsHaskell;

      librarySystemDepends = depsSystem;

      passthru = {
        devShell = haskellPackages.shellFor {
          packages = _: [ drv ];
          buildInputs = depsSystem;
          nativeBuildInputs = depsTools;
        };

        inherit ghcVersion haskellPackages;
      };
    }
  )
)
