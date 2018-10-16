{}:

let
  overlays = [(import ./overlay.nix)];
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2018-09-29";
    url  = https://github.com/nixos/nixpkgs/;
    rev  = "46651b82b87318e37440c15a639d49ec05e79b79";
  }) { inherit overlays; };
in {oanda = pkgs.haskellPackages.oanda; }
