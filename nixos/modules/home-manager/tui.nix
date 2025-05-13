{ pkgs, ... }:
{
  home.packages = with pkgs; [
    binsider # analyse and edit ELF binaries
    fzf # fuzzy file finder; desired by yazi
    yazi # file manager
    zoxide # smarter cd; desired by yazi
  ];
}
