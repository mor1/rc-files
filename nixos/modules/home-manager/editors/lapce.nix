{ pkgs, ... }:
{
  programs.lapce = {
    enable = false;

    keymaps = [
      {
        command = "palette.command";
        key = "Alt+x";
      }
      # {
      #   command = "open_log_file";
      #   key = "Ctrl+Shift+L";
      # }
    ];

    plugins = [
      # based on metadata from https://plugins.lapce.dev/. note that author (at
      # least) is case-sensitive.
      {
        author = "foxlldev";
        name = "solarized";
        version = "0.1.1";
        hash = "sha256-649WE9BcR80HwKnEYEM3O5EgfAiKb/cZFM3bXjjf854=";
      }
      {
        author = "abreumatheus";
        name = "lapce-ruff-lsp";
        version = "0.1.1";
        hash = "sha256-ttYWC+GEQlFZR+Qu2rMLiKFDNsFeILRzDl6XQeiqqt0=";
      }
      {
        author = "dzhou121";
        name = "lapce-rust";
        version = "0.3.2162";
        hash = "sha256-hFKEMJt8lio/kuuZTDEshZ6NBjpDM65VoS6hl1CTSZ0=";
      }
      {
        author = "panekj";
        name = "lapce-toml";
        version = "0.0.0";
        hash = "sha256-hSXo5d7DuresKfN8lDlC8SCJ/+NeWZcAH8Xbp3kUwNc=";
      }
      {
        author = "xiaoxin-sky";
        name = "lapce-rome";
        version = "0.0.1";
        hash = "sha256-fChbExavT24DVvtqQMmXdrlvqTUoaUb5WtQly82PSiY=";
      }
      {
        author = "X64D";
        name = "lapce-tinymist";
        version = "0.0.1";
        hash = "sha256-iSudWviA2XX0yhbdq8UVyeWXLh9TCIAdRQwjGWrfD54=";
      }
      {
        author = "MrFoxPro";
        name = "lapce-nix";
        version = "0.0.1";
        hash = "sha256-n+j8p6sB/Bxdp0iY6Gty9Zkpv9Rg34HjKsT1gUuGDzQ=";
      }
    ];

    settings = {
      core = {
        custom-titlebar = false;
        color-theme = "Solarized Dark";
        icon-theme = "Material Icons";
      };
      editor = {
        font-family = "Atkinson Hyperlegible Mono, monospace";
        font-size = 14;
        tab-width = 2;
        cursor-surrounding-lines = 4;
        render-whitespace = "all";
        bracket-pair-colorization = true;
        highlight-matching-brackets = true;
      };
      ui = {
        font-size = 14;
        open-editors-visible = false;
      };
      # lapce-nix.lsp-path = "$\{pkgs.nil\}/bin/nil";
    };
  };
}
