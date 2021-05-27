args:

let
  # master on 2021-05-27
  rev = "8ed03bc71243f642a5d47cfc9b84b0592cf5d158";
  sha256 = "1s1vizai81fpgrsila3mkxrq1grinj0k76608msvl6id99jn3rr7";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
