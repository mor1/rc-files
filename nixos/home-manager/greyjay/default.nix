# swaymsg workspace back_and_forth
# swipe gestures

{ pkgs, lib, ... }:
let
  username = "mort";
in
{

  imports = [
    ../../modules/home-manager/cli.nix
    ../../modules/home-manager/dev.nix
    ../../modules/home-manager/editors
    ../../modules/home-manager/gui.nix
    ../../modules/home-manager/media.nix
    ../../modules/home-manager/system.nix
    ../../modules/home-manager/texlive.nix
    ../../modules/home-manager/typst.nix
    ../../modules/home-manager/tui.nix
    ../../modules/home-manager/unfree.nix
  ];

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
  };

  programs.home-manager.enable = true;

  systemd.user.services."org-sync" = {
    Unit = {
      Description = "org sync";
    };
    Service = {
      Type = "oneshot";
      ExecStart = toString (
        pkgs.writeShellScript "org-sync.sh" ''
          set -eou pipefail

          PATH=$PATH:${lib.makeBinPath [ pkgs.coreutils ]}

          EXPORTBASE='/home/mort/Dropbox/calendar'
          EXPORT="richard-incoming.org richard.org richard-tripit.org"

          IMPORTBASE='/home/mort/Dropbox/people/family.org'
          IMPORT="angela.org david.org eleanor.org william.org \
            birthdays.org car.org home.org solemnities.org"

          for f in $EXPORT; do
            cp -a "$EXPORTBASE/$f" "$IMPORTBASE/$f"
          done

          for f in $IMPORT; do
            cp -a "$IMPORTBASE/$f" "$EXPORTBASE/$f"
          done
        ''
      );
    };
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.timers."org-sync" = {
    Unit.Description = "org sync scheduler";
    Timer = {
      OnUnitActiveSec = "2m";
    };
    Install.WantedBy = [ "timers.target" ];
  };

  home.stateVersion = "24.05";
}
