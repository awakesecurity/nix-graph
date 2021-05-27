let
  pkgs = import ./nix/pkgs.nix;

  nix-graph = pkgs.haskellPackages.nix-graph;

in
nix-graph.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
