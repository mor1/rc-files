{ inputs, lib, config, pkgs, options, ... }:

let
  root_dev = "/dev/disk/by-uuid/c3cc9248-ade1-4b94-9e6e-d50990171471";
  home_dev = "/dev/disk/by-uuid/3bbf38b9-06f1-4df3-b29c-3d2a863088cc";
  swap_dev = "/dev/disk/by-uuid/223bf99d-f6e2-41b0-b30f-7e1064e308df";
  hostname = "greyjay";
in {
  # setup configuration, home-manager, flake
  imports = [
    ./hardware-configuration.nix
    ./cambridge-vpn
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
    inputs.home-manager.nixosModules.home-manager
  ];

  # home-manager = {
  #   extraSpecialArgs = { inherit inputs; };
  #   users.mort = import ./home-manager;
  # };

  nix = {
    # flakes
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # housekeeping
    settings.auto-optimise-store = true;
    gc.automatic = true;
  };

  # system packages
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "memtest86-efi" ];
  security.polkit.enable = true;
  services.udisks2.enable = true;
  environment.systemPackages = with pkgs; [
    cifs-utils # samba
    git # obviously
    ifuse # ios optional; to mount using 'ifuse'
    keyd # key remappings
    libimobiledevice # ios
    lxqt.lxqt-policykit # for gvfs
    restic # backups
    vim # i just don't like nano, ok?
  ];

  # boot via UEFI
  boot = {
    initrd.luks.devices = {
      cryptroot = {
        device = "${root_dev}";
        preLVM = true;
        allowDiscards = true;
      };
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
        consoleMode = "1";
        memtest86.enable = true;
      };
    };

    supportedFilesystems = [ "ntfs" ];
  };

  # mount home and swap
  fileSystems = {
    "/home" = {
      device = "${home_dev}";
      fsType = "ext4";
    };

    "/mnt/home-desktop" = {
      device = "//desktop-bqgpfcm/14mort/";
      fsType = "cifs";
      options = let
        automount_opts =
          "x-systemd.automount,noauot,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
      in [ "${automount_opts},credentials=/etc/secrets/smb-secrets" ];
    };
  };

  swapDevices = [{ device = "${swap_dev}"; }];

  # networking, plus UCAM timeservers
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
      enableStrongSwan = true;
    };

    # https://discourse.nixos.org/t/ntp-use-values-from-network-manager-via-dhcp/23408/2
    timeServers = options.networking.timeServers.default ++ [
      "ntp0.cam.ac.uk"
      "ntp1.cam.ac.uk"
      "ntp2.cam.ac.uk"
      "ntp3.cam.ac.uk"
    ];
  };

  # keyboard and locale
  i18n.defaultLocale = "en_GB.UTF-8";
  console.keyMap = "uk";

  # audio
  sound.enable = true;
  sound.mediaKeys.enable = true;
  nixpkgs.config.pulseaudio = true;
  hardware.pulseaudio.enable = true;

  # system services
  services = {
    # getty.autologinUser = "mort";

    automatic-timezoned.enable = true;
    localtimed.enable = true;
    dbus = {
      enable = true;
      packages = with pkgs; [ networkmanager strongswanNM ];
    };
    geoclue2.enable = true;
    gnome.gnome-keyring.enable = true;

    gvfs.enable = true;

    keyd = {
      enable = true;
      keyboards.default = {
        ids = [ "*" ];
        settings = {
          main = {
            # capslock -> (held) ctrl, (tap) ESC
            capslock = "overload(control, esc)";
          };
          shift = {
            grave = "G-4"; # S-` -> €
          };
        };
      };
    };

    onedrive.enable = true;

    # podgrab.enable = true;
    printing.enable = true;
  };

  # system applications
  programs = {
    sway.enable = true;
    vim.defaultEditor = true;
    wireshark.enable = true;
  };

  xdg.portal = {
    # https://nixos.wiki/wiki/Sway
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # setup users
  users.users = {
    root = { extraGroups = [ "wheel" ]; };

    mort = {
      isNormalUser = true;
      extraGroups = [ "audio" "docker" "video" "wheel" "wireshark" ];
    };

  };

  # restic backups not as root; https://nixos.wiki/wiki/Restic
  # users.users.restic.isNormalUser = true;
  # security.wrappers.restic = {
  #   source = "${pkgs.restic.out}/bin/restic";
  #   owner = "restic";
  #   group = "users";
  #   permissions = "u=rwx,g=,o=";
  #   capabilities = "cap_dac_read_search=+ep";
  # };

  # automount USB storage devices on plugin
  services = {
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEMS=="usb", SUBSYSTEM=="block", \
        ENV{ID_FS_USAGE}=="filesystem", \
        RUN{program}+= "${pkgs.systemd}/bin/systemd-mount --no-block -AG $devnode"
    '';
    restic = {
      backups = let
        backup = target: {
          initialize = false;
          repository = "local:/run/media/system/backup-${target}";
          passwordFile = "/etc/secrets/restic-password-backup-${target}";
          user = "root";

          timerConfig = {
            OnCalendar = "hourly";
            Persistent = true;
          };

          paths = [ "/home/mort" "/var/lib/NetworkManager" "/etc/secrets" ];

          exclude = [
            "/home/**/.venv/"
            "/home/**/__pycache__"
            "/home/**/node_modules/"
            "/home/**/target/"
            "/home/**/vendor/"
            "/home/*/.cache"
            "/home/*/.local/share/Trash"
            "/home/*/.local/share/containers"
            "/home/*/.npm"
            "/home/*/Downloads"
            "/home/mort/Dropbox (Maestral)/"
            "/home/mort/OneDrive/"
            "/home/mort/keybase"
            "/home/mort/l/"
          ];
        };
      in {
        backup-home = backup "home";
        backup-christs = backup "christs";
        backup-wgb = backup "wgb";
      };
    };
  };

  # docker
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  # iphone/ipad
  services.usbmuxd = {
    enable = true;
    # package = pkgs.usbmuxd2;
  };

  system.stateVersion = "23.11";
}
