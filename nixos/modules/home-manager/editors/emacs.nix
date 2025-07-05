{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = (epkgs: [ epkgs.mu4e ]);
  };
  services.emacs.enable = true;
}
