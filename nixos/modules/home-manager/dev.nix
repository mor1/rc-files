{ pkgs, ... }:
{
  home.packages =
    with pkgs;
    let
      man_pages = [
        man-pages
        man-pages-posix
      ];
      bash_tools = [
        bash-language-server # LSP for bash
        shellcheck # linting for bash
        shfmt # ba/sh code formatting
      ];
      python_tools = [
        basedpyright
        python313
        ruff
        uv
      ]
      ++ (with python313Packages; [
        pygments
        python-lsp-server
      ]);
      ocaml_tools = [
        dune_3
        gcc
        ocaml
        ocamlformat
        opam
      ]
      ++ (with ocamlPackages; [
        cmdliner
        findlib
        merlin
        ocaml-lsp
        ocp-indent
        utop
      ]);
    in
    [
      gh # github CLI
      git # obviously
      git-filter-repo # for fixing up repos
      git-lfs # large file support
      gnumake # unavoidably
      jq # pretty-print JSON
      nixfmt-rfc-style # format .nix files
      rustup # manage Rust installations
    ]
    ++ man_pages
    ++ python_tools
    ++ ocaml_tools
    ++ bash_tools;

  programs = {
    git.enable = true;

    opam = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
