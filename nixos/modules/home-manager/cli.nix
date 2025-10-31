{ pkgs, ... }:
{
  home.packages =
    with pkgs;
    let
      cli = [
        aria2 # feature rich wget/curl
        # kelpsget # https://github.com/davimf721/KelpsGet
        stow # manage dotfiles via symlinks
        cryfs # encrypted filesystem image support
        dateutils # fiddle with dates
        dig # because DNS
        doxx # view .docx in terminal
        exiftool # manipulate images
        fastfetch # improved `neofetch` system info display
        file # identify filetype by magic
        get_iplayer # download from iPlayer
        ghostscript # ps etc
        handlr # manage XDG Open mappings
        html-tidy # tidy HTML
        imagemagick # image manipulation tools
        inetutils
        inotify-tools # commands using inotify/dnotify APIs
        internetarchive # internet archive
        jdupes # file duplicate finder
        jhead # jpeg exif header manipulation tool
        lsof # list open file handles
        lynx # cli web browser
        mupdf # PDF manipulation
        offlineimap # download emails
        pandoc # document processing and conversion
        pass # password manager
        pdfcpu # `optimize` for PDF optimisation
        pdftk # more PDF manipulation
        qpdf # yet more PDF manipulation
        subversion # hysterical raisins
        sysstat # system stats
        traceroute
        tree # tree-format recursive ls
        unzip # what it says on the tin, because zip can't unzip
        wget # network downloader
        which # locate command in $PATH
        yt-dlp # youtube download
        zip # what it says on the tin
        zola # static site generation
      ];
      nu_posix = [
        # flash # fast inotify replacement, https://github.com/sage-scm/flash/
        bat # better cat
        biff # better date
        bottom # btm ~ better top, htop, etc
        broot # interactive directory navigation
        brush # rusty bash
        chafa # terminal graphics viewer
        ctpv # terminal file previewer
        cyme # better `lsusb`
        delta # better syntax highlighting diff
        dotter # manage dotfiles by copying via a config.toml
        dua # disk usage, interactively
        eza # improved `ls`
        fd # `find` replacement
        fend # better CLI calculator
        hexyl # hex pretty printer
        htop # graphical top
        iotop # io top
        jujutsu # better git
        just # updated gnumake replacement
        procs # better ps
        ripgrep # rg ~ `grep` replacement
        sudo-rs # memory-safe `sudo`
        uutils-coreutils-noprefix # replaces GNU `coreutils`
        uutils-findutils
        viddy # better watch
      ];
    in
    cli ++ nu_posix;

  programs = {
    bash = {
      enable = false;
      enableCompletion = false;
    };

    command-not-found.enable = false;
    nix-index = {
      enable = true;
      enableBashIntegration = true;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    lf = {
      enable = false;

      previewer = {
        keybinding = "i";
        source = "${pkgs.ctpv}/bin/ctpv";
      };

      extraConfig = ''
        &${pkgs.ctpv}/bin/ctpv -s $id
        cmd on-quit %${pkgs.ctpv}/bin/ctpv -e $id
        set cleaner ${pkgs.ctpv}/bin/ctpvclear
      '';
    };

    mu.enable = true;
  };
}
