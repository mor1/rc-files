{ pkgs, ... }:
{
  programs.zed-editor = {
    enable = true;

    extensions = [
      "git-firefly"
      "just"
      "make"
      "nix"
      "ocaml"
      "ruff"
      "toml"
    ];

    extraPackages = with pkgs; [
      nil # LSP formatter for Nix language
      nixd # LSP for Nix language
      package-version-server # quell a warning about a missing package
    ];

    userSettings = {
      telemetry.metrics = false;

      # kill the AIs
      assistant.enabled = false;
      features.copilot = false;

      # colour themes
      theme = {
        mode = "dark";
        light = "Solarized Light";
        dark = "Solarized Dark";
      };
      experimental.theme_overrides = {
        editor.background = "#333";
      };

      # gui setup
      show_whitespaces = "all";
      relative_line_numbers = true;
      inlay_hints.enabled = true;

      # fonts
      ui_font_size = 14;
      ui_font_family = "Hack Nerd Font";
      ui_font_features.calt = true;

      buffer_font_size = 12;
      buffer_font_family = "Hack Nerd Font";

      # terminal configuration
      terminal = {
        alternate_scroll = "off";
        blinking = "off";
        copy_on_select = false;
        dock = "bottom";
        detect_venv.on = {
          directories = [
            ".venv"
            "venv"
          ];
          activate_script = "default";
        };
        end.EDITOR = "zed --wait";
        font_family = "Hack Nerd Font";
        font_size = 14;
        line_height = "comfortable";
        option_as_meta = true;
        button = false;
        shell = "system";
        toolbar = {
          title = false;
        };
        working_directory = "current_project_directory";
      };
      load_direnv = "shell_hook";

      # keyboard mappings
      vim_mode = false;
      base_keymap = "Emacs";

      # LSP setup
      lsp = {
        nix.binary.path_lookup = true;
        package-version-server.binary.path = "package-version-server";
        rust-analyzer.binary.path_lookup = true;
      };

      # languages setup
      languages = {
        Nix = {
          formatter.external = {
            command = "nixfmt";
            arguments = [
              "--strict"
              "--verify"
              "--"
            ];
          };
        };

        Python = {
          language_servers = [ "ruff" ];
          format_on_save = "on";
          formatter = [
            {
              code_actions = {
                "source.fixAll.ruff" = true;
                "source.organizeImports.ruff" = true;
              };
            }
            { language_server.name = "ruff"; }
          ];
        };
      };
    };
  };
}
