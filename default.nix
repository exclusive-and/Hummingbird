{...} @ initArgs:
import ./nix/build-haskell initArgs (
  self: {
    birds = {
      version = "0";
      src = ./code;
      buildInputs = hackage: with hackage; [
        base
        containers
        text
      ];
    };
  }
)
