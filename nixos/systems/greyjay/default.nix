{
  inputs,
  lib,
  pkgs,
  options,
  ...
}:

let
  root_dev = "/dev/disk/by-uuid/c3cc9248-ade1-4b94-9e6e-d50990171471";
  home_dev = "/dev/disk/by-uuid/3bbf38b9-06f1-4df3-b29c-3d2a863088cc";
  swap_dev = "/dev/disk/by-uuid/223bf99d-f6e2-41b0-b30f-7e1064e308df";
  hostname = "greyjay";
  username = "mort";
in
{
  # setup configuration, home-manager, flake
  imports = [
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
    ../../modules/nixos/cambridge-vpn
  ];

  home-manager = {
    # backupFileExtension = "backup"; # disable: better to see the failure
    extraSpecialArgs = { inherit inputs; };
    users.${username} = import ../../home-manager/${hostname};
  };

  nix = {
    # flakes
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # housekeeping
    settings.auto-optimise-store = true;
    gc.automatic = true;
  };

  # system packages
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "memtest86-efi" ];
  security.polkit.enable = true;
  services.udisks2.enable = true;
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };

    systemPackages = with pkgs; [
      cifs-utils # samba
      ifuse # ios optional; to mount using 'ifuse'
      keyd # key remappings
      krb5 # kerberos
      libimobiledevice # ios
      lxqt.lxqt-policykit # for gvfs
      openssh_gssapi # ssh client tools that support GSS API for kerberos tickets
      restic # backups
      vim # i just don't like nano, ok?
    ];
  };

  boot = {
    # kernelPackages = pkgs.linuxKernel.packages.linux_6_11;
    initrd.luks.devices = {
      cryptroot = {
        device = "${root_dev}";
        preLVM = true;
        allowDiscards = true;
      };
    };

    loader = {
      efi.canTouchEfiVariables = false; # true on first invocation
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
        consoleMode = "1";
        memtest86.enable = true;
        # windows = {
        #   "10" = {
        #     efiDeviceHandle = "HD0b";
        #     title = "Windows 10";
        #   };
        # };
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
      device = "//desktop-bqgpfcm/14mor/";
      fsType = "cifs";
      options =
        let
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,user,users";
        in
        [ "${automount_opts},credentials=/etc/secrets/smb.secrets,uid=1000,gid=100" ];
    };
  };

  swapDevices = [ { device = "${swap_dev}"; } ];

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

  # audio & bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  # system services
  services = {
    automatic-timezoned.enable = true;
    localtimed.enable = true;

    dbus = {
      enable = true;
      packages = with pkgs; [
        gcr
        networkmanager
        strongswanNM
      ];
    };

    geoclue2.enable = true;

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

    avahi = {
      enable = true;
      nssmdns4 = true;
      # openFirewall = true;
    };

    printing = {
      enable = true;
      clientConf = ''
        ServerName cups-serv.cl.cam.ac.uk
        User rmm1002
      '';
    };

    # automount USB storage devices on plugin
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEMS=="usb", SUBSYSTEM=="block", \
        ENV{ID_FS_USAGE}=="filesystem", \
        RUN{program}+= "${pkgs.systemd}/bin/systemd-mount --no-block -AG $devnode"
    '';

    # backups
    restic = {
      backups =
        let
          backup = target: {
            initialize = false;
            repository = "local:/run/media/system/backup-${target}";
            passwordFile = "/etc/secrets/restic-password-backup-${target}";
            user = "root";

            timerConfig = {
              OnCalendar = "hourly";
              Persistent = true;
            };

            paths = [
              "/home/mort"
              "/var/lib/NetworkManager"
              "/etc"
              "/etc/secrets"
            ];

            exclude = [
              "/home/**/.venv/"
              "/home/**/__pycache__"
              "/home/**/node_modules/"
              "/home/**/target/"
              "/home/**/vendor/"
              "/home/*/.cache"
              "/home/*/.cargo"
              "/home/*/.local/share/Trash"
              "/home/*/.local/share/containers"
              "/home/*/.mozilla"
              "/home/*/.npm"
              "/home/*/Downloads"
              "/home/mort/keybase"
              "/home/mort/l/"
            ];
          };
        in
        {
          backup-home = backup "home";
          backup-christs = backup "christs";
          backup-wgb = backup "wgb";
        };
    };
  };

  # system applications
  programs = {
    sway.enable = true;
    vim = {
      enable = true;
      defaultEditor = true;
    };
    wireshark.enable = true;
  };

  xdg.portal = {
    # https://nixos.wiki/wiki/Sway
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
      xdg-desktop-portal-gtk
    ];
    # gtkUsePortal = true;
    wlr.enable = true;
  };

  # setup users
  users.users = {
    root = {
      extraGroups = [ "wheel" ];
    };

    mort = {
      isNormalUser = true;
      extraGroups = [
        "audio"
        "docker"
        "video"
        "wheel"
        "wireshark"
      ];
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

  # docker
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  services = {
    pcscd.enable = true;
    usbmuxd.enable = true; # iphone/ipad
  };

  # kerberos for cambridge
  security.krb5.settings.config = ''
    [libdefaults]
    forwardable = true
    default_realm = DC.CL.CAM.AC.UK
  '';

  # use sudo-rs rather than sudo
  security.sudo-rs = {
    enable = true;
    execWheelOnly = true;
    wheelNeedsPassword = true;
  };

  # enable local fontDir for unpackaged font install
  fonts.fontDir.enable = true;

  system.stateVersion = "24.05";
}
