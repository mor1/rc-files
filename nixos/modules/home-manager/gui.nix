{ pkgs, lib, ... }:
{
  imports = [
    ./sway.nix
    ./vscode.nix
  ];

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "corefonts" # some fonts
      "slack" # slack, electron wrapper
      "vscode"
      "skypeforlinux" # skype, such as it is
      "vista-fonts" # vista-fonts here but vistafonts for install?!
      "zoom" # zoom here but zoom-us for install?!
    ];

  home.packages =
    with pkgs;
    let
      apps = [
        alacritty # alternative xterm
        chromium # teams calling in browser doesn't work in firefox
        firefox # web browser
        gnome.nautilus # maybe the least sucky of the file managers, so far?
        gst123
        gst_all_1.gstreamer
        gst_all_1.gst-libav
        inkscape # vector graphics editing
        keybase-gui # keybase
        # libav_12
        ffmpeg_7-full
        libreoffice # ~ms office
        meld # compare files / folders
        networkmanagerapplet # nm-connection-manager, NetworkManager GUI
        okular # pdf viewer / annotator
        pdfpc # pdf presentation viewer
        signal-desktop # signal private messaging
        skypeforlinux # skype
        slack # slack
        teams-for-linux # ms teams in electron
        thunderbird # email
        wire-desktop # wire private messaging
        zoom-us # zoom vc
      ];

      media = [
        digikam
        greg
        imv
        kodi
        rhythmbox
        vlc
      ];

      fonts = [
        (nerdfonts.override { fonts = [ "Hack" ]; })
        corefonts
        vistafonts
      ];
    in
    apps ++ media ++ fonts;

  fonts.fontconfig.enable = true;

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = [ "okularApplication_pdf.desktop" ];
      "application/x-extension-htm" = [ "firefox.desktop" ];
      "application/x-extension-html" = [ "firefox.desktop" ];
      "application/x-extension-shtml" = [ "firefox.desktop" ];
      "application/x-extension-xht" = [ "firefox.desktop" ];
      "application/x-extension-xhtml" = [ "firefox.desktop" ];
      "application/xhtml+xml" = [ "firefox.desktop" ];
      "image/jpeg" = [ "imv.desktop" ];
      "image/jpg" = [ "imv.desktop" ];
      "image/png" = [ "imv.desktop" ];
      "image/svg+xml" = [ "imv.desktop" ];
      "text/html" = [ "firefox.desktop" ];
      "x-scheme-handler/about" = [ "firefox.desktop" ];
      "x-scheme-handler/chrome" = [ "firefox.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "x-scheme-handler/msteams" = [ "teams-for-linux.desktop" ];
      "x-scheme-handler/unknown" = [ "firefox.desktop" ];
    };
  };

  programs = {
    chromium = {
      enable = true;
    };

    firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
    };
  };
}
