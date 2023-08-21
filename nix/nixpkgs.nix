args:

let
  # master on 2023-08-21
  rev = "e2b5418ca2e684f185708bdbb3aaa82cdd86a266";
  sha256 = "sha256:1nr7xswxd1gk7isxmg8smn76cxbxamxddh1xap46n5rkf3j0l8pr";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
