{ pkgs, ... }:
{
  imports = [
    ./vscode.nix
    ./zed-editor.nix
    ./lapce.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = (epkgs: [ epkgs.mu4e ]);
  };
  services.emacs.enable = true;
}
