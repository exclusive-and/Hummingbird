initArgs @ {
  buildPackages ? import ./init.nix initArgs
, ...
}:
let
  inherit (buildPackages)
    buildHs
    cabal
    ghcHackage
    lib;

  # Build a single Haskell Cabal package from the input specification.
  buildGoal =
    { pname
    , version
    , src
    , license ? ""
    , buildInputs ? _: []
    }:
    buildHs {
      inherit pname version src license;
      libraryHaskellDepends = buildInputs ghcHackage;
    };

  # Returns whether a derivation is a member of a list of derivations.
  isDrvIn =
    x:
    lib.all (drv: x.drvPath or null == drv.drvPath);
  
  # Returns whether a derivation IS NOT a member of a list of derivations.
  isDrvNotIn = x: drvs: !(isDrvIn x drvs);

  # Get the combined set of external Cabal dependencies for a set of derivations that
  # I plan to build.
  getManyCabalDeps =
    drvs:
    builtins.zipAttrsWith
      (_: builtins.concatMap (builtins.filter (x: isDrvNotIn x drvs)))
      (builtins.map (drv: drv.getCabalDeps) drvs);

  # Make a dev shell for a Haskell project, containing all the relevant dependencies.
  makeShell =
    deps:
    extraTools:
    let
      placeholderArgs = {
        pname = "hask-shell";
        version = "0";
        license = null;
      };

      placeholder = buildHs (placeholderArgs // deps);
      placeholderDrv = placeholder.envFunc {};
    in
      placeholderDrv.overrideAttrs (oldAttrs: {
        nativeBuildInputs = [
          cabal
          buildPackages.hls
        ] ++ oldAttrs.nativeBuildInputs ++ extraTools ghcHackage;
      });

  # Build many Haskell packages at once. I calculate the fixpoint of the final set,
  # so that members can depend on other members of the same project.
  buildManyGoals =
    goalsFn: rec
    {
      # The set of built derivations.
      outputs = lib.fix (
        self:
        builtins.mapAttrs
          (pname: goal: buildGoal (goal // { inherit pname; }))
          (goalsFn self)
      );

      # The combined set of external Cabal dependencies for the whole project.
      getCabalDeps = getManyCabalDeps (builtins.attrValues outputs);

      # The dev shell for this project.
      devShell =
        {
          extraTools ? _: []
        }:
        makeShell getCabalDeps extraTools;
    };
in
  buildManyGoals
