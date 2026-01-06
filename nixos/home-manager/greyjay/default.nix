{ pkgs, lib, ... }:
let
  username = "mort";
in
{
  imports = [
    ../../modules/home-manager/cli.nix
    ../../modules/home-manager/dev.nix
    ../../modules/home-manager/editors
    ../../modules/home-manager/gui.nix
    ../../modules/home-manager/system.nix
    ../../modules/home-manager/texlive.nix
    ../../modules/home-manager/typst.nix
    ../../modules/home-manager/tui.nix
    ../../modules/home-manager/unfree.nix
  ];

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
  };

  programs.home-manager.enable = true;

  home.stateVersion = "24.05";
}
