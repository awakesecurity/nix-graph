pkgsFinal: pkgsPrev:

{
  haskellPackages = pkgsPrev.haskellPackages.override (old: {
    overrides =
      pkgsPrev.lib.composeExtensions
        (old.overrides or (_: _: { }))
        (haskellPackagesFinal: haskellPackagesPrev: {
          nix-graph = haskellPackagesPrev.callCabal2nix "nix-graph" ../. { };
        });
  });
}
