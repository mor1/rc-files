{ pkgs, lib, ... }: {

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    davmail
    direnv
    gnupg
    hunspell # spellchecking and dictionaries
    hunspellDicts.en_GB-large
    maestral
    onedrive
    pciutils
    sshfs
    stow
    strongswan
    usbutils
    zola # static site generation
  ];

  programs = {
    keychain = {
      enable = true;
      agents = [ "gpg" "ssh" ];
    };
  };

  systemd.user.services.kbfs.Service.PrivateTmp = lib.mkForce false;

  services = {
    gpg-agent = {
      # reduce the typing of credentials
      enable = true;
      enableBashIntegration = true;

      defaultCacheTtl = 2592000;
      maxCacheTtl = 2592000;
    };

    keybase.enable = true;

    kbfs = {
      enable = true;
      # enableRedirector = true;
    };
  };
}
