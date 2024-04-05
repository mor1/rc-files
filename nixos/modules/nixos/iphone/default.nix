# https://gist.githubusercontent.com/danbst/1aed84dd0f5fe465dfca9319c6e63df5/raw/1b53b337d6f32ab6782442cc6ac338062f085c85/iphone.nix
# First add this module to your /etc/nixos/configuration.nix
# ...
#   imports = [ /path/to/iphone.nix ];
#   iphone.enable = true;
#   iphone.user = "yourusername";
# ...
# Then rebuild system. Attach iPhone via cable, open terminal and run command `iphone`
# It will fail, but there will occure a dialog on your iPhone to "trust this computer"
# Press OK there and run `iphone` again. If it succeeds it will open a freshly mounted folder

{ config, pkgs, lib, ... }:
let cfg = config.iphone;
in {
  options.iphone = {
    enable = lib.mkOption { default = false; };
    directory = lib.mkOption { default = "/run/media/iPhone"; };
    user = lib.mkOption { };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.libimobiledevice
      pkgs.usbmuxd
      (pkgs.writeScriptBin "iphone" ''
        sudo systemctl restart iphone \
         && ${pkgs.gnome2.libgnome}/bin/gnome-open ${cfg.directory}
      '')
    ];
    services.usbmuxd.enable = true;
    services.usbmuxd.user = cfg.user;

    systemd.services.iphone = {
      preStart =
        "mkdir -p ${cfg.directory}; chown ${cfg.user} ${cfg.directory}";
      script = ''
        ${pkgs.libimobiledevice}/bin/idevicepair pair \
        && exec ${pkgs.ifuse}/bin/ifuse ${cfg.directory}
      '';
      serviceConfig = {
        PermissionsStartOnly = true;
        User = cfg.user;
        Type = "forking";
      };
    };
  };
}
