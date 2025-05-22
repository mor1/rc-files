{ pkgs, ... }:
{
  home.packages = with pkgs; [
    tinymist
    typst
    typstfmt
  ];
}
