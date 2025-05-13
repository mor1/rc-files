{ pkgs, ... }:
{
  imports = [
    ./emacs.nix
    ./lapce.nix
    ./vscode.nix
    ./zed-editor.nix
  ];
}
