args@{
  sources ? import ./nix/sources.nix
, getnix ? import sources."get.nix" {}
, isShell ? false
}:
getnix.buildHaskell {
  name = "birds";
  version = "0";
  depends = hspkgs: with hspkgs; [
    base
    containers
    text
  ];
  inherit isShell;
}
