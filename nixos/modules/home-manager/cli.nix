{ pkgs, ... }: {

  home.packages = with pkgs;
    let
      cli = [
        bc # calculator
        dig # because DNS
        exiftool # manipulate images
        file # identify filetype by magic
        get_iplayer # download from iPlayer
        ghostscript # ps etc
        handlr # manage XDG Open mappings
        pandoc # document processing and conversion
        imagemagick # image manipulation tools
        inetutils
        jhead # jpeg exif header manipulation tool
        keychain # cli to manage SSH, GPG keys
        lynx # cli web browser
        mupdf # PDF manipulation
        pdftk # more PDF manipulation
        qpdf # yet more PDF manipulation
        subversion # hysterical raisins
        sysstat # system stats
        traceroute
        tree # tree-format recursive ls
        typst # better latex?
        unzip # what it says on the tin, because zip can't unzip
        wget # network downloader
        which # locate command in $PATH
        zip # what it says on the tin
      ];
      nu_posix = [
        bat # better cat
        bottom # btm ~ better top, htop, etc
        broot # interactive directory navigation
        chafa # terminal graphics
        ctpv # terminal file previewer
        delta # better syntax highlighting diff
        dua # disk usage, interactively
        eza # improved `ls`
        fd # `find` replacement
        fzf # fuzzy file finder; desired by yazi
        hexyl # hex pretty printer
        htop # graphical top
        lf # file manager
        jujutsu # better git
        just # updated gnumake replacement
        mcfly # better shell history
        nushell # maybe it's time to kick another addiction
        procs # better ps
        ripgrep # rg ~ `grep` replacement
        uutils-coreutils-noprefix # replaces GNU `coreutils`
        viddy # better watch
        zoxide # smarter cd; desired by yazi
      ];
    in cli ++ nu_posix;

  programs.lf = {
    enable = true;

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
}
