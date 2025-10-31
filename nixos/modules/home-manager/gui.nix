{ pkgs, lib, ... }:
{
  imports = [ ./sway.nix ];

  home.packages =
    with pkgs;
    let
      apps = [
        czkawka # detect file duplication, empty directories, &c
        # ghostty # alternative gui terminal
        inkscape # vector graphics editing
        keybase-gui # keybase
        libreoffice # ~ms office
        # nautilus # maybe the least sucky of the file managers, so far?
        kdePackages.okular # pdf viewer / annotator
        signal-desktop # signal private messaging
        slack # slack
        teams-for-linux # ms teams in electron
        zoom-us # zoom vc
      ];

      fonts = [
        atkinson-hyperlegible-mono
        atkinson-hyperlegible-next
        corefonts
        gyre-fonts
        nerd-fonts.hack
        powerline-symbols
        vista-fonts
      ];

      themes = [ foot.themes ];
    in
    apps ++ fonts ++ themes;

  fonts.fontconfig.enable = true;

  xdg.mimeApps = {
    enable = true;
    defaultApplications =
      let
        targets =
          app: tgts:
          builtins.listToAttrs (
            map (t: {
              name = "${t}";
              value = [ "${app}" ];
            }) tgts
          );

        imgapp = "imv.desktop";
        imgs = [
          "image/jpeg"
          "image/jpg"
          "image/png"
          "image/svg+xml"
        ];

        pdfapp = "okularApplication_pdf.desktop";
        pdfs = [ "application/pdf" ];

        webapp = "firefox.desktop";
        webs = [
          "application/x-extension-htm"
          "application/x-extension-html"
          "application/x-extension-shtml"
          "application/x-extension-xht"
          "application/x-extension-xhtml"
          "application/xhtml+xml"
          "text/html"
          "x-scheme-handler/about"
          "x-scheme-handler/chrome"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/unknown"
        ];
      in
      {
        "x-scheme-handler/msteams" = [ "teams-for-linux.desktop" ];
      }
      // targets imgapp imgs
      // targets pdfapp pdfs
      // targets webapp webs;
  };

  programs = {
    chromium.enable = true;

    foot = {
      enable = true;
      settings = {
        main = {
          font = "Atkinson Hyperlegible Mono Medium:size=8";
          include = "${pkgs.foot.themes}/share/foot/themes/selenized-dark";
        };
        scrollback.lines = 10000;
      };
    };

    firefox.enable = true;

    rio = {
      enable = false;
      settings = {
        cursor = "▇";
        blinking-cursor = false;
        fonts = {
          family = "Hack Nerd Font";
          size = 12;
        };
        # navigation.mode = "Breadcrumb";
        keyboard.use-kitty-keyboard-protocol = true;
        ## theme = "Solarized Dark";
        colors = {
          ## Solarized Dark
          background = "#002b36";
          foreground = "#bbbbbb"; # 839496";
          selection-background = "#073642";
          selection-foreground = "#93a1a1";
          cursor = "#839496";
          black = "#073642";
          red = "#dc322f";
          green = "#859900";
          yellow = "#b58900";
          blue = "#268bd2";
          magenta = "#d33682";
          cyan = "#2aa198";
          white = "#eee8d5";
          light_black = "#002b36";
          light_red = "#cb4b16";
          light_green = "#586e75";
          light_yellow = "#657b83";
          light_blue = "#839496";
          light_magenta = "#6c71c4";
          light_cyan = "#93a1a1";
          light_white = "#fdf6e3";
        };
      };
    };
  };
}
