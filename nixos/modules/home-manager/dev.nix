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
      nix_tools = [
        # nix-cli # unified nix command line tooling
        nix-du # show disk usage of roots
        nix-tree # show dependency tree of derivations https://github.com/utdemir/nix-tree
        nixd # nix LSP server
        nixfmt-rfc-style # format .nix files
      ];
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
    in
    [
      dockerfile-language-server
      gh # github CLI
      git # obviously
      git-filter-repo # for fixing up repos
      git-lfs # large file support
      gnumake # unavoidably
      harper # grammar checker for devs
      jq # pretty-print JSON
      rustup # manage Rust installations
      tombi
    ]
    ++ bash_tools
    ++ man_pages
    ++ nix_tools
    ++ ocaml_tools
    ++ python_tools;

  programs = {
    git.enable = true;

    jujutsu = {
      enable = true;
      settings.user = {
        email = "mort@cantab.net";
        name = "Richard Mortier";
      };
    };

    opam = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
