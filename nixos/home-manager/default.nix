{ nixpkgs, pkgs, lib, config, ... }:

let
  username = "mort";
  background = "~/rc-files/floatlg.jpg";
in {
  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
  };

  # setup home-manager
  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "slack" "skypeforlinux" ];

  home.packages = with pkgs;
    let
      system_apps = [
        (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
        coreutils
        davmail
        direnv
        gnupg
        maestral
        stow
        strongswan
      ];
      cli_apps = [
        bc
        file
        htop
        imagemagick
        inetutils
        jhead
        mc
        nixos-option
        texlive.combined.scheme-full
        tree
        unzip
        wget
        which
      ];
      dev_apps =
        [ emacs29 fd git gnumake jq lapce nixfmt ocaml opam ripgrep rustup ];
      fonts =
        [ font-awesome_4 hack-font material-design-icons powerline-fonts ];
      sway_apps = [
        brightnessctl
        gammastep
        gnome3.adwaita-icon-theme
        grim
        kanshi
        slurp
        swayosd
        wdisplays
        wev
      ];
      gui_apps = [
        firefox
        libreoffice
        signal-desktop
        skypeforlinux
        slack
        thunderbird
        vocal
        wire-desktop
      ];
      media_apps = [ greg quodlibet-full vlc ];
      # brave evolution evolution-ews mailspring
    in system_apps ++ cli_apps ++ dev_apps ++ fonts ++ sway_apps ++ gui_apps
    ++ media_apps;

  fonts.fontconfig.enable = true;

  wayland.windowManager.sway = {
    enable = true;

    swaynag.enable = true;

    wrapperFeatures.gtk = true;

    config = rec {
      modifier = "Mod4"; # use WIN not ALT-L for sway controls
      focus.wrapping = "force";
      workspaceAutoBackAndForth = true;

      # build the startup script to start apps in workspaces
      startup = let
        msg = cmds: "swaymsg '${builtins.concatStringsSep ", " cmds}' ";
        after = delay: cmds: "sleep ${toString delay} && ${msg cmds}";
        now = after 0;

        #  # name the displays
        # set $laptop eDP-1
        # set $hdmi HDMI-A-1

        # # arrange displays: laptop at bottom, offset left of HDMI
        # ${msg [ "output $laptop pos 0 2160" "output $hdmi pos 640 0" ]}
        startup = pkgs.writeShellScriptBin "startup.sh" ''

          # 2:mail
          ${now [
            "workspace --no-auto-back-and-forth 2:mail"
            "exec firefox -P richard.mortier@gmail.com"
            "exec firefox -P mort@ikva.ai"
            "exec firefox -P rmm1002@cam.ac.uk"
            "layout stacking"
          ]}

          # 3:chat
          ${after 5 [
            "workspace --no-auto-back-and-forth 3:chat"
            "exec slack"
          ]}
          ${after 1 [ "splith" "exec signal-desktop" ]}
          ${after 3 [ "[class=Signal] focus" "splitv" "exec skypeforlinux" ]}

          # 4:media
          ${after 3 [
            "workspace --no-auto-back-and-forth 4:media"
            "exec quodlibet"
            # "exec firefox -P default --new-window http://localhost:8080/"
          ]}

          # 1 (default)
          ${after 1 [
            "workspace --no-auto-back-and-forth 1" # output $hdmi $laptop"
            "exec emacs -f todo"
          ]}
          ${after 1 [ "splith" "exec foot" ]}
          ${after 1 [ "splitv" "exec firefox -P default" ]}

          # reload config; sometimes the bar is confused...
          ${after 2 [ "reload" ]}
        '';
      in [{ command = "${startup}/bin/startup.sh"; }];

      # all my keyboards are GB layout
      input = {
        "*" = { xkb_layout = "gb"; };
        "touchpad" = {
          natural_scroll = "enabled";
          tap = "enabled";
        };
      };

      # additional keybindings; cannot simply remap input ev -> output ev
      keybindings = lib.mkOptionDefault {
        # F1
        "XF86AudioMute" = "exec swayosd --output-volume mute-toggle";
        # F2
        "XF86AudioLowerVolume" = "exec  swayosd --output-volume lower";
        # F3
        "XF86AudioRaiseVolume" = "exec swayosd --output-volume raise";
        # # F4
        # "XF86AudioMicMute" = ''exec swayosd --input-volume mute-toggle''; # XXX broken

        # F5
        "XF86MonBrightnessDown" =
          "exec brightnessctl s 10%-"; # swayosd --brightness lower''; # XXX broken
        # F6
        "XF86MonBrightnessUp" =
          "exec brightnessctl s 10%+"; # swayosd --brightness raise''; # XXX broken

        # F7  XF86Display
        # F8  XF86WLAN
        # F9  XF86Messenger
        # F10 XF86Go
        # F11 Cancel
        # F12 XF86Favorites
      };

      # status bars using i3status-rust
      bars = let status = "${pkgs.i3status-rust}/bin/i3status-rs";
      in [
        {
          position = "top";
          statusCommand = "${status} ~/.config/i3status-rust/config-top.toml";
        }
        {
          position = "bottom";
          statusCommand =
            "${status} ~/.config/i3status-rust/config-bottom.toml";
          workspaceButtons = false;
        }
      ];
    };

    # set background
    extraConfig = ''output "*" bg ${background} fill'';
  };

  services = {
    #   swayosd = {
    #     enable = true;
    #     maxVolume = 120;
    #   };

    emacs = {
      # until the addiction is kicked
      package = pkgs.emacs29;
      enable = true;
      client.enable = true;
      defaultEditor = true;
    };

    gammastep = {
      # alter brightness and redshift based on time and location
      enable = true;
      dawnTime = "6:00-8:00";
      duskTime = "20:00-22:00";
      provider = "geoclue2";
      temperature.day = 6000;
      temperature.night = 3500;
    };

    gpg-agent = {
      # reduce the typing of credentials
      enable = true;
      enableBashIntegration = true;

      defaultCacheTtl = 2592000;
      maxCacheTtl = 2592000;
    };

    kanshi = {
      # autodetect and arrange external monitors
      enable = true;
      profiles = let
        laptop = "eDP-1";
        hdmi = "HDMI-A-1";
        move_ws = w: o: ''
          ${pkgs.sway}/bin/swaymsg workspace {w}, move workspace to output ${o}
        '';
      in {
        undocked = { outputs = [{ criteria = "${laptop}"; }]; };
        docked = {
          outputs = [
            {
              criteria = "${laptop}"; # 3840x2400
              position = "0,1662";    # below ${hdmi} => y = 2160 / 1.3
              scale = 2.0;
            }
            {
              criteria = "${hdmi}"; # 3840x2160
              position = "832,0";   # offset-right 1/3 laptop => 3840/2*1.3 / 3
              scale = 1.3;
            }
          ];
          exec = [ "${move_ws 1 hdmi}" "${move_ws "3:chat" hdmi}" ];
        };
      };
    };

    swayidle =
      # screen saving and locking
      let
        lock = "${pkgs.swaylock}/bin/swaylock -C ~/.config/swaylock/config";
        suspend = "systemctl suspend";
      in {
        enable = true;
        timeouts = [
          {
            timeout = 300;
            command = "${lock}";
          }
          {
            timeout = 600;
            command = "${suspend}";
          }
        ];
        events = [
          {
            event = "before-sleep";
            command = "${lock}";
          }
          {
            event = "lock";
            command = "${lock}";
          }
        ];
      };

  };

  programs = {

    direnv = {
      # per-directory env configuration
      enable = true;
      enableBashIntegration = true;
      nix-direnv = { enable = true; };
    };

    firefox = {
      # everybody needs a web browser these days
      enable = true;
      package = pkgs.firefox-wayland;
      # profiles.default.extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      #   privacy-badger
      #   https-everywhere
      # ];
    };

    git.enable = true; # obviously

    i3status-rust = {
      # providing information for status bars
      enable = true;
      bars = {
        top = {
          theme = "solarized-dark";
          icons = "awesome4";
          blocks = let
            caffeine_on = {
              icon = "pomodoro_break";
              text = "";
            };
            caffeine_off = {
              icon = "resolution";
              text = "";
            };
          in [
            {
              block = "battery";
              format = " $icon $percentage {$time |}";
              charging_format = " $icon $percentage ";
              full_format = " $icon $percentage {$time |}";
            }
            {
              block = "memory";
              format = "$icon$mem_used_percents [$swap_used_percents]";
            }
            {
              block = "cpu";
              format = "$icon$utilization";
              interval = 1;
            }
            {
              block = "load";
              format = "[$1m]";
              interval = 1;
            }
            {
              block = "sound";
              headphones_indicator = true;
            }
            { block = "music"; }
            {
              block = "custom";
              format = "{ $icon|} $text.pango-str()";
              json = true;
              interval = "once";
              command = ''
                swaymsg -q inhibit_idle none && printf '${
                  builtins.toJSON caffeine_off
                }
                ' '';
              cycle = [
                ''
                  swaymsg -q inhibit_idle none && printf '${
                    builtins.toJSON caffeine_off
                  }
                  ' ''
                ''
                  swaymsg -q inhibit_idle visible && printf '${
                    builtins.toJSON caffeine_on
                  }
                  ' ''
              ];
            }
            {
              block = "time";
              format = "$timestamp.datetime(f:'%c')";
              interval = 2;
            }
          ];
        };

        bottom = {
          theme = "solarized-dark";
          icons = "awesome4";
          blocks = [
            {
              block = "net";
              format =
                " $icon $device {$ip |NO IPv4 }{$ipv6 |NO IPv6 }{($signal_strength $ssid)|}";
            }
            {
              block = "external_ip";
              format = "$ip $country_flag";
            }
            {
              block = "disk_space";
              info_type = "available";
              interval = 60;
              path = "/";
              format = "$icon $path:$available";
              warning = 10.0;
              alert = 5.0;
              alert_unit = "GB";
            }
            {
              block = "disk_space";
              info_type = "available";
              format = "$path:$available";
              interval = 60;
              path = "~";
              warning = 20.0;
              alert = 10.0;
              alert_unit = "GB";
            }
            { block = "backlight"; }
          ];
        };
      };
    };

    opam = {
      # OCaml support
      enable = true;
      enableBashIntegration = true;
    };

    swaylock = {
      # screen locking driven by swayidle
      enable = true;
      settings = {
        color = "000000";
        daemonize = true;
        ignore-empty-password = true;
        image = "${background}";
      };
    };
  };

  home.stateVersion = "23.05";
}
