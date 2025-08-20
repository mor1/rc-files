{ pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    gcr
    gnupg
    hunspell # spellchecking and dictionaries
    hunspellDicts.en_GB-large # en.GB
    maestral # dropbox client
    pciutils
    sshfs # mount remote filesystems over ssh
    usbutils
  ];

  programs = {
    keychain = {
      enable = true;
      enableBashIntegration = true;
    };
  };

  systemd.user.services.kbfs.Service.PrivateTmp = lib.mkForce false;

  services = {
    gnome-keyring.enable = true;

    gpg-agent = {
      # reduce the typing of credentials
      enable = true;
      enableBashIntegration = true;
      enableSshSupport = true;

      defaultCacheTtl = 2592000;
      maxCacheTtl = 2592000;
      pinentry.package = pkgs.pinentry-gnome3;
    };

    keybase.enable = true;

    kbfs.enable = true;
  };
}
