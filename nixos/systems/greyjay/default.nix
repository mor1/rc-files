{
  inputs,
  lib,
  pkgs,
  options,
  ...
}:

let
  hostname = "greyjay";
  root_partition = "/dev/disk/by-uuid/c3cc9248-ade1-4b94-9e6e-d50990171471";
  username = "mort";

  coreutils-full-name =
    "coreuutils-full"
    + builtins.concatStringsSep "" (
      builtins.genList (_: "_") (builtins.stringLength pkgs.coreutils-full.version)
    );

  coreutils-name =
    "coreuutils"
    + builtins.concatStringsSep "" (
      builtins.genList (_: "_") (builtins.stringLength pkgs.coreutils.version)
    );

  findutils-name =
    "finduutils"
    + builtins.concatStringsSep "" (
      builtins.genList (_: "_") (builtins.stringLength pkgs.findutils.version)
    );

  diffutils-name =
    "diffuutils"
    + builtins.concatStringsSep "" (
      builtins.genList (_: "_") (builtins.stringLength pkgs.diffutils.version)
    );
in
{
  # setup configuration, home-manager, flake
  imports = [
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
    ../../modules/nixos/cambridge-vpn
  ];

  system.replaceDependencies.replacements = [
    {
      oldDependency = pkgs.coreutils-full;
      newDependency = pkgs.symlinkJoin {
        name = coreutils-full-name;
        paths = [ pkgs.uutils-coreutils-noprefix ];
      };
    }
    {
      oldDependency = pkgs.coreutils;
      newDependency = pkgs.symlinkJoin {
        name = coreutils-name;
        paths = [ pkgs.uutils-coreutils-noprefix ];
      };
    }
    {
      oldDependency = pkgs.findutils;
      newDependency = pkgs.symlinkJoin {
        name = findutils-name;
        paths = [ pkgs.uutils-findutils ];
      };
    }
    # {
    #   oldDependency = pkgs.diffutils;
    #   newDependency = pkgs.symlinkJoin {
    #     name = diffutils-name;
    #     paths = [ pkgs.uutils-diffutils ];
    #   };
    # }
  ];

  home-manager = {
    # backupFileExtension = "backup"; # disable: better to see the failure
    users.${username} = import ../../home-manager/${hostname};
  };

  nix = {
    # flakes
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # housekeeping
    settings = {
      auto-optimise-store = true;
      download-buffer-size = 128 * 1024 * 1024;
      trusted-users = [ "@wheel" ];
    };
    gc.automatic = true;
  };

  # system packages
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "memtest86-efi" ];
  security.polkit.enable = true;
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      ## XXX new, to test
      WLR_DRM_NO_MODIFIERS = "1";
      WLR_RENDERER = "vulkan";
      XDG_CURRENT_DESKTOP = "sway";
      MOZ_ENABLE_WAYLAND = "1";
      QT_QPA_PLATFORM = "wayland";
      CLUTTER_BACKEND = "wayland";
      SDL_VIDEODRIVER = "wayland";
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
    initrd = {
      systemd.enable = true;
      luks.devices = {
        cryptroot = {
          device = "${root_partition}";
          preLVM = true;
          allowDiscards = true;
        };
      };
    };

    loader = {
      efi.canTouchEfiVariables = false; # true on first invocation
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
        consoleMode = "auto";
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

  # networking, plus UCAM timeservers
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
      plugins = with pkgs; [ networkmanager-strongswan ];
      wifi.powersave = true;
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
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
    # raopOpenFirewall = true;
    wireplumber.enable = true;
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
            capslock = "overloadt2(control, esc, 150)";
            rightalt = "leftalt";
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
  users = {
    users = {
      root = {
        extraGroups = [ "wheel" ];
      };

      mort = {
        isNormalUser = true;
        extraGroups = [
          "audio"
          "docker"
          "input"
          "lpadmin"
          "networkmanager"
          "video"
          "wheel"
          "wireshark"
        ];
      };

      # run restic backups not as root; https://nixos.wiki/wiki/Restic
      restic = {
        group = "restic";
        isSystemUser = true;
      };
    };
    groups = {
      restic = { };
      lpadmin = { };
    };
  };

  # automount USB storage devices on plugin
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEMS=="usb", SUBSYSTEM=="block", \
      ENV{ID_FS_USAGE}=="filesystem", \
      RUN{program}+= "${pkgs.systemd}/bin/systemd-mount --no-block -AG $devnode"
  '';
  services.udisks2.enable = true;

  # backups
  security.wrappers.restic = {
    source = lib.getExe pkgs.restic;
    owner = "restic";
    group = "restic";
    permissions = "u=rx,g=,o=";
    capabilities = "cap_dac_read_search=+ep";
  };
  services.restic = {
    backups =
      let
        backup = target: {
          initialize = false;
          repository = "local:/run/media/system/backup-${target}/RESTIC";
          passwordFile = "/etc/secrets/restic-password-backup-${target}";
          user = "restic";
          package = pkgs.writeShellScriptBin "restic" ''
            exec /run/wrappers/bin/restic "$@"
          '';

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

  # docker
  virtualisation.docker = {
    enable = false;
    enableOnBoot = false;
  };

  services = {
    pcscd.enable = true;
    usbmuxd.enable = true; # iphone/ipad
  };

  security = {
    # auditing
    auditd.enable = false;

    # kerberos for cambridge
    krb5.settings.config = ''
      [libdefaults]
      forwardable = true
      default_realm = DC.CL.CAM.AC.UK
    '';

    # use sudo-rs rather than sudo
    sudo-rs = {
      enable = true;
      execWheelOnly = true;
      wheelNeedsPassword = true;
    };
  };
  # enable local fontDir for unpackaged font install
  fonts.fontDir.enable = true;

  system.stateVersion = "24.05";
}
