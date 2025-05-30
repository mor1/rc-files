{ pkgs, ... }:
{
  home.packages = with pkgs; [
    fzf # fuzzy file finder; desired by yazi
    yazi # file manager
    zoxide # smarter cd; desired by yazi
  ];

  programs = {
    bash = {
      enable = false;
      enableCompletion = false;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    git.enable = true;
    mu.enable = true;
  };
}
