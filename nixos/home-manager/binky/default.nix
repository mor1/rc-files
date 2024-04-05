{ ... }:
let username = "rmm1002";
in {

  imports = [
    ../../modules/home-manager/system.nix
    ../../modules/home-manager/tui.nix
    ../../modules/home-manager/gui.nix
  ];

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
  };

  programs.home-manager.enable = true;

  home.stateVersion = "24.05";
}
