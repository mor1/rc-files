{ pkgs, ... }: {
  home.packages = with pkgs; [
    tinymist
    typship
    typst
    typstyle
  ];
}
