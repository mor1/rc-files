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
  environment.systemPackages = with pkgs; [ vim ];

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
    interception-tools = {
      # https://wiki.archlinux.org/title/Interception-tools
      enable = true;
      plugins = [ pkgs.interception-tools-plugins.caps2esc ];
      udevmonConfig =
        # https://github.com/NixOS/nixpkgs/issues/126681#issuecomment-860071968
        # future? https://gitlab.com/interception/linux/plugins/dual-function-keys
        let intercept = "${pkgs.interception-tools}/bin/intercept";
        in let uinput = "${pkgs.interception-tools}/bin/uinput";
        in let
          caps2esc = "${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc";
        in ''
          - JOB: "${intercept} -g $DEVNODE | ${caps2esc} -m 1 -t 10000 | ${uinput} -d $DEVNODE"
            DEVICE:
              EVENTS:
                EV_KEY: [ KEY_CAPSLOCK ]
        '';
    };

    # getty.autologinUser = "mort";
    automatic-timezoned.enable = true;
    dbus.packages = [ pkgs.networkmanager pkgs.strongswanNM ];
    geoclue2.enable = true;
    gnome.gnome-keyring.enable = true;
    # onedrive.enable = true;
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

  system.stateVersion = "23.05";
}
