{ pkgs, ... }: {

  home.packages = with pkgs;
    let
      python_tools = [ hatch python312 ruff uv ]
        ++ (with python312Packages; [ autopep8 pip pygments ]);
      ocaml_tools = [ gcc ocaml dune_3 ocamlformat opam ]
        ++ (with ocamlPackages; [
          cmdliner
          findlib
          merlin
          ocaml-lsp
          ocp-indent
          utop
        ]);
    in [
      gh # github CLI
      git # obviously
      git-filter-repo # for fixing up repos
      git-lfs # large file support
      gnumake # unavoidably
      jq # pretty-print JSON
      nil # LSP for Nix language
      nixfmt # format .nix files
      rustup # manage Rust installations
    ] ++ python_tools ++ ocaml_tools;

  programs = {
    opam = {
      enable = true;
      enableBashIntegration = true;
    };

  };
}
