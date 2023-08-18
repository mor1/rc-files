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
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users.mort = import ./home-manager;
  };

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
  environment.systemPackages = with pkgs; [ keyd restic vim ];

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
  fileSystems."/home" = {
    device = "${home_dev}";
    fsType = "ext4";
  };

  swapDevices = [{ device = "${swap_dev}"; }];

  # networking, plus UCAM timeservers
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
      plugins = [ pkgs.networkmanager_strongswan ];
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
    dbus.packages = [ pkgs.networkmanager pkgs.strongswanNM ];
    geoclue2.enable = true;
    gnome.gnome-keyring.enable = true;

    keyd = {
      enable = true;
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

    printing.enable = true;
    strongswan-swanctl.enable = true;
  };

  # system applications
  programs = {
    vim.defaultEditor = true;
    sway.enable = true;
  };

  # setup users
  users.users = {
    root = { extraGroups = [ "wheel" ]; };

    mort = {
      isNormalUser = true;
      extraGroups = [ "wheel" "video" ];
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

  services.restic = {
    backups = {
#      package = pkgs.restic;
#      package.CGO_ENABLED = 0;
      full = {
        initialize = true;
        repository = "local:/mnt/backup-hdd";
        # XXX can't by symlinked path
        passwordFile = "/etc/secrets/restic-password";
        user = "mort";

        timerConfig = {
          OnCalendar = "daily";
          Persistent = true;
        };

        # backupPrepareCommand = ''
        #   # remove stale locks
        #   ${pkgs.restic}/bin/restic unlock || true
        # '';

        paths = [
          "/home/mort"
          "/var/lib"
        ];

        exclude = [
          "/var/lib/systemd"
          "/var/lib/containers"
          "/var/lib/docker"
          "/home/*/.local/share/Trash"
          "/home/*/.cache"
          "/home/*/Downloads"
          "/home/*/.npm"
          "/home/*/.local/share/containers"
        ];
      };
    };
  };

  system.stateVersion = "23.05";
}