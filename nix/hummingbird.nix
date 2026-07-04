{
  mkDerivation
, lib

, cabal-install
, haskell-language-server
, shellFor

, ansi-terminal
, base
, binary
, brick
, bytestring
, constraints-extras
, containers
, crypton_1_1_2
, dependent-sum
, dependent-sum-template
, directory
, filepath
, exceptions_0_10_12
, hashable
, haskeline
, monad-chronicle
, mtl
, parsec
, prettyprinter
, prettyprinter-ansi-terminal
, primitive
, ram
, shellmet
, some
, template-haskell
, text
, text-rope
, these
, transformers
, unordered-containers
, vty
}:

lib.fix (hummingbird:
  mkDerivation {
    pname = "hummingbird";
    src = ../.;
    version = "0";
    isExecutable = true;
    isLibrary = true;
    executableHaskellDepends = [
      base
      crypton_1_1_2
      directory
      exceptions_0_10_12
      filepath
      mtl
      text
      prettyprinter
      prettyprinter-ansi-terminal
    ];
    libraryHaskellDepends = [
      ansi-terminal
      base
      binary
      brick
      bytestring
      constraints-extras
      containers
      crypton_1_1_2
      dependent-sum
      dependent-sum-template
      filepath
      exceptions_0_10_12
      hashable
      haskeline
      monad-chronicle
      mtl
      parsec
      prettyprinter
      prettyprinter-ansi-terminal
      primitive
      ram
      shellmet
      some
      template-haskell
      text
      text-rope
      these
      transformers
      unordered-containers
      vty
    ];
    passthru.devShell = shellFor {
      packages = _: [
        hummingbird
      ];
      buildInputs = [
        cabal-install
        haskell-language-server
      ];
      nativeBuildInputs = [
        cabal-install
      ];
    };
    homepage = "https://github.com/exclusive-and/Hummingbird";
  }
)
