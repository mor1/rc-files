# swaymsg workspace back_and_forth
# swipe gestures

## crib:
# pushd ~/rc-files/nixos/ ; sudo nix-channel --update ; nix flake update ; sudo nixos-rebuild switch --upgrade-all ; popd
# sudo nix-collect-garbage -d && nix-collect-garbage -d && sudo nix-store --gc && nix-store --gc && sudo nixos-rebuild switch
# pushd ~/rc-files/nixos ; nixfmt configuration.nix flake.nix home-manager/default.nix ; popd
# pushd ~/u/src/dl-solar ; ./dl-solar yesterday ; popd

{ ... }:
let
  username = "mort";
in
{

  imports = [
    ../../modules/home-manager/system.nix
    ../../modules/home-manager/tui.nix
    ../../modules/home-manager/media.nix
    ../../modules/home-manager/gui.nix
    ../../modules/home-manager/texlive.nix
  ];

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
  };

  programs.home-manager.enable = true;

  home.stateVersion = "24.05";
}
