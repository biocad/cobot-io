let
  bcd-lts = import (builtins.fetchGit {
    url = "git@github.com:biocad/nix-lts.git";
    ref = "master";
  });
in
bcd-lts.mkBiocadProject {
  src = bcd-lts.pkgs.haskell-nix.haskellLib.cleanGit { name = "cobot-io"; src = ./.; };
  shellArgs = {
    buildInputs = [ bcd-lts.pkgs.RNA ];
  };
}
