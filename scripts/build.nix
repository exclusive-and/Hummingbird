{
  # The version of GHC that will be used for the build.
  ghcVersion ? "ghc912"
, lib
, nixpkgs
, haskellPackages ? nixpkgs.haskell.packages.${ghcVersion}
, ...
}@outerArgs:

{
  name
, pname ? name
, src
, version
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
      inherit pname src version;
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
