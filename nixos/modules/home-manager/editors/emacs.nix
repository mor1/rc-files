{ pkgs, ... }:
{
  home.packages = with pkgs; [ emacs-lsp-booster ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages =
      epkgs: with epkgs; ([
        corfu
        jinx
        mu4e
        nerd-icons-corfu
        pretty-sha-path
        tree-sitter-langs
        (treesit-grammars.with-grammars (p: [
          p.tree-sitter-bash
          p.tree-sitter-elisp
          p.tree-sitter-markdown
          p.tree-sitter-markdown-inline
          p.tree-sitter-nix
          p.tree-sitter-ocaml
          p.tree-sitter-python
          p.tree-sitter-rust
          p.tree-sitter-toml
          p.tree-sitter-typst
          p.tree-sitter-yaml
        ]))
      ]);
  };
  services.emacs.enable = true;
}
