{ pkgs, ... }:
{
  home.packages = with pkgs; [
    fzf # fuzzy file finder; desired by yazi
    yazi # file manager
    zoxide # smarter cd; desired by yazi
  ];
}
