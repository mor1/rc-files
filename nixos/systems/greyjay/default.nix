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
    extraSpecialArgs = {
      inherit inputs;
    };
    users.${username} = import ../../home-manager/${hostname};
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
      pavucontrol # graphical control of AV routing
      restic # backups
      vim # i just don't like nano, ok?
    ];
  };

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

  # audio
  security.rtkit.enable = true;
  services.pipewire = {
    alsa.enable = true;
    alsa.support32Bit = true;
    enable = true;
    jack.enable = true;
    pulse.enable = true;

    extraConfig = {
      pipewire."92-low-latency" = {
        context.properties = {
          default.clock.rate = 48000;
          default.clock.quantum = 32;
          default.clock.min-quantum = 32;
          default.clock.max-quantum = 32;
        };
      };
      pipewire-pulse."92-low-latency" = {
        context.modules = [
          {
            name = "libpipewire-module-protocol-pulse";
            args = {
              pulse.min.req = "32/48000";
              pulse.default.req = "32/48000";
              pulse.max.req = "32/48000";
              pulse.min.quantum = "32/48000";
              pulse.max.quantum = "32/48000";
            };
          }
        ];
        stream.properties = {
          node.latency = "32/48000";
          resample.quality = 1;
        };
      };
    };
  };

  # system services
  services = {
    automatic-timezoned.enable = true;
    localtimed.enable = true;
    dbus = {
      enable = true;
      packages = with pkgs; [
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
              "/home/*/.local/share/Trash"
              "/home/*/.local/share/containers"
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

  # iphone/ipad
  services.usbmuxd = {
    enable = true;
    # package = pkgs.usbmuxd2;
  };

  # kerberos for cambridge
  security.krb5.settings.config = ''
    [libdefaults]
    forwardable = true
    default_realm = DC.CL.CAM.AC.UK
  '';

  system.stateVersion = "24.05";
}
