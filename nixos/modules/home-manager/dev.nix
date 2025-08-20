{ pkgs, ... }:
{
  home.packages =
    with pkgs;
    let
      man_pages = [
        man-pages
        man-pages-posix
      ];
      python_tools = [
        python312
        ruff
        uv
      ]
      ++ (with python312Packages; [
        autopep8
        pip
        pygments
      ]);
      ocaml_tools = [
        gcc
        ocaml
        dune_3
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
    man_pages
    ++ [
      gh # github CLI
      git # obviously
      git-filter-repo # for fixing up repos
      git-lfs # large file support
      gnumake # unavoidably
      jq # pretty-print JSON
      nixfmt-rfc-style # format .nix files
      rustup # manage Rust installations
    ]
    ++ python_tools
    ++ ocaml_tools;

  programs = {
    git.enable = true;

    opam = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
