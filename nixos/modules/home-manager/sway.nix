{ pkgs, lib, ... }:
let
  background = "/home/mort/rc-files/floatlg.jpg";
  homews = "1";
  chatws = "2:chat";
  mailws = "3:mail";
  otherws = [
    "4"
    "5"
    "6"
    "7"
  ];
  codews = "8:code";
  mediaws = "9:media";

  # `screen` description is formed of "{.make} {.model} {.serial}" from the raw
  # JSON output, `swaymsg -r -t get_outputs`; alternatively use `.name` though
  # that tends to name the port, e.g., `hDMI-A-1`

  # `sink`/`source` is a mess because Linux sound, notably (old) PulseAudio vs
  # (new) Pipewire, and the inconsistent tools; TODO FIXME
  laptop = {
    screen = "eDP-1";
    # screen = "California Institute of Technology 0x1403 Unknown";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Speaker__sink";
    source = "alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Mic1__source";
    # card = "alsa_card.pci-0000_00_1f.3-platform-skl_hda_dsp_generic";
    # profile = "HiFi (HDMI1, HDMI2, HDMI3, Mic1, Mic2, Speaker)";
  };

  # my offices
  wgb = {
    # screen = "HDMA-A-1";
    screen = "LG Electronics LG HDR 4K 0x0005DD99";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__HDMI1__sink";
    source = "alsa_input.usb-046d_HD_Pro_Webcam_C920_C18974EF-02.analog-stereo";
  };
  christs = {
    # screen = "HDMI-A-1";
    screen = "LG Electronics LG HDR 4K 0x00035DAC";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Headphones__sink";
    source = "alsa_input.usb-046d_Logitech_Webcam_C925e_8EA2331F-02.analog-stereo";
    # card = "alsa_card.pci-0000_00_1f.3-platform-skl_hda_dsp_generic";
    # profile = "HiFi (HDMI1, HDMI2, HDMI3, Mic1, Mic2, Speaker)";
  };

  # public spaces
  fn05 = {
    # screen = "HDMIA-A-1";
    screen = "Sony SONY TV  *07 0x01010101";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__HDMI1__sink";
  };
  nms_a = {
    # screen = "HDMI-A-1";
    screen = "Crestron Electronics, Inc. Crestron Unknown";
  };

  # at home
  tv = {
    # screen = "HDMI-A-1";
    screen = "Panasonic Industry Company Panasonic-TV 0x01010101";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__HDMI1__sink";
  };
  amp = {
    # screen = "HDMI-A-1";
    screen = "ONKYO Corporation TX-SR608 Unknown";
  };

  modifier = "Mod4";
  swayfonts = {
    names = [
      "Atkinson Hyperlegible Mono"
      "PowerlineSymbols"
    ];
    style = "Medium";
    size = 8.0;
  };
in
{
  imports = [ ./theme.nix ];

  home.packages = with pkgs; [
    brightnessctl # control screen brightness
    gammastep # automatically dim+redden screen at night
    kanshi # modify sway config on hardware changes
    pwvucontrol # graphical control of AV routing (pipewire)
    slurp # select a compositor region to stdout
    shotman # screenshotting
    wdisplays # gui for display configuration
    wev # wayland event viewer
    wl-clipboard # pipe to/from clipboard
    wlclock # old skool analogue
    xorg.xset # used by vlc via xdg-screensaver to manage screensaver timeouts
  ];

  wayland.windowManager.sway = {
    enable = true;
    checkConfig = false;
    xwayland = true;

    swaynag.enable = true;

    wrapperFeatures.gtk = true;

    config = {
      fonts = swayfonts;
      #  // {
      #   names = [ "Mono" ];
      # };

      modifier = "${modifier}"; # use WIN not ALT-L for sway controls
      focus.wrapping = "force";
      workspaceAutoBackAndForth = true;

      # build the startup script to start apps in workspaces
      startup =
        let
          msg = cmds: "swaymsg '${builtins.concatStringsSep ", " cmds}'";
          workspace = ws: msg [ "workspace --no-auto-back-and-forth ${ws}" ];
          after = delay: cmds: "sleep ${toString delay} && ${msg cmds}";
          startup = pkgs.writeShellScriptBin "startup.sh" ''
            wait_for () {
              { swaymsg -r -m -t subscribe '["window"]' |
                jq -c --unbuffered '. | select(.change == "new")' |
                { grep -m1 . >/dev/null ; pkill swaymsg ;} &
              } 2>/dev/null
              pid=$!
              swaymsg -- "exec $*" && sleep 0.5
              wait $pid 2>/dev/null
            }

            ${workspace "${mediaws}"}
            wait_for "rhythmbox"

            ${workspace "${codews}"}
            wait_for firefox -P github.com
            # wait_for zeditor
            ${after 3 [ "layout stacking" ]}

            ${workspace "${mailws}"}
            wait_for firefox -P richard.mortier@gmail.com
            wait_for firefox -P 14mortier@gmail.com
            wait_for firefox -P mort@ikva.ai
            wait_for firefox -P rmm1002@cam.ac.uk
            wait_for teams-for-linux
            ${after 3 [ "layout stacking" ]}

            ${workspace "${chatws}"}
            wait_for slack
            ${after 1 [ "split horizontal" ]}

            # some signal weirdness prevents the window appearing until a second
            # copy is run, and immediately exits on detecting it's the second instance
            wait_for "signal-desktop & sleep 3 && signal-desktop"

            ${workspace "${homews}"}
            wait_for emacsclient -c -s /tmp/emacs-mort/server
            ${after 1 [ "split horizontal" ]}
            wait_for foot
            ${after 1 [ "split vertical" ]}
            wait_for firefox -P default

            ${after 1 [
              "reload"
              "exec systemctl --user daemon-reload"
              "exec systemctl --user import-environment"
              "exec systemctl restart --user kanshi.service"
              "exec systemctl restart --user maestral-daemon@maestral.service"
            ]}
          '';
        in
        [ { command = "${startup}/bin/startup.sh"; } ];

      # all my keyboards are GB layouta
      input = {
        "*" = {
          xkb_layout = "gb";
        };
        "type:touchpad" = {
          natural_scroll = "enabled";
          tap = "enabled"; # click on tap
          tap_button_map = "lmr"; # 1 finger = left click, 2 = middle, 3 = right
          dwt = "enabled"; # disable touchpad while typing
          dwtp = "enabled"; # disable touchpard while track pointing
        };
        "pointer" = {
          accel_profile = "adaptive";
        };
      };

      output."*".bg = "${background} fill";

      # additional keybindings; cannot simply remap input ev -> output ev
      keybindings =
        let
          swaylock = "${pkgs.swaylock}/bin/swaylock -C ~/.config/swaylock/config";
          swayosd = lib.getExe' pkgs.swayosd "swayosd-client";

          f1 = "exec ${swayosd} --max-volume 130 --output-volume mute-toggle";
          f2 = "exec ${swayosd} --max-volume 130 --output-volume lower";
          f3 = "exec ${swayosd} --max-volume 130 --output-volume raise";
          f4 = "exec ${swayosd} --input-volume mute-toggle";
          f5 = "exec brightnessctl -e s 10%-";
          f6 = "exec brightnessctl -e s 10%+";
          f7 = "exec ${swaylock}";
          net_toggle = pkgs.writeShellScriptBin "net_toggle.sh" ''
            if [[ $(nmcli n) =~ enabled ]]; then
              nmcli n off
            else
              nmcli n on
            fi
          '';
          f8 = "exec ${net_toggle}/bin/net_toggle.sh";
          f9 = "exec rhythmbox-client --play-pause";
          f10 = "exec rhythmbox-client --stop";
          f11 = "exec rhythmbox-client --previous";
          f12 = "exec rhythmbox-client --next";
        in
        lib.mkOptionDefault {
          ## bare function keys
          "F1" = f1;
          "F2" = f2;
          "F3" = f3;
          "F4" = f4;
          "F5" = f5;
          "F6" = f6;
          "F7" = f7;
          "F8" = f8;
          "F9" = f9;
          "F10" = f10;
          "F11" = f11;
          "F12" = f12;

          ## bluetooth headset
          "XF86AudioPause" = f9;
          "XF86AudioPlay" = f9;
          "XF86AudioPrev" = f11;
          "XF86AudioNext" = f12;

          ## thinkpad keyboard
          "XF86AudioMute" = f1;
          "XF86AudioLowerVolume" = f2;
          "XF86AudioRaiseVolume" = f3;
          "XF86AudioMicMute" = f4;
          "XF86MonBrightnessDown" = f5;
          "XF86MonBrightnessUp" = f6;
          "XF86Display" = f7;
          "XF86WLAN" = f8;
          "XF86NotificationCenter" = f9;
          "XF86PickupPhone" = f10;
          "XF86HangupPhone" = f11;
          "XF86Favorites" = f12;

          ## MSFT keyboard
          "Help" = f1;
          "Undo" = f2;
          "Redo" = f3;
          "XF86New" = f4;
          "XF86Open" = f5;
          "XF86Close" = f6;
          "XF86Reply" = f7;
          "XF86MailForward" = f8;
          "XF86Send" = f9;
          "XF86SpellCheck" = f10;
          "XF86Save" = f11;
          "Print" = f12; # also catches PrtSc on thinkpad

          ## extras, all keyboards
          "${modifier}+Shift+l" = "exec ${swaylock}";

          "${modifier}+p" = "exec shotman --capture window";
          "${modifier}+Shift+p" = "exec shotman --capture region";
          "${modifier}+Ctrl+p" = "exec shotman --capture output";
        };

      # status bars using i3status-rust
      bars =
        let
          status = "${pkgs.i3status-rust}/bin/i3status-rs";
        in
        [
          {
            position = "top";
            fonts = swayfonts;
            statusCommand = "${status} ~/.config/i3status-rust/config-top.toml";
          }
          {
            position = "bottom";
            fonts = swayfonts;
            statusCommand = "${status} ~/.config/i3status-rust/config-bottom.toml";
            workspaceButtons = false;
          }
        ];
    };

    extraConfig = ''
      bindswitch --reload --locked lid:on output ${laptop.screen} disable
      bindswitch --reload --locked lid:off output ${laptop.screen} enable
    '';
  };

  services = {

    gammastep = {
      # alter brightness and redshift based on time and location
      enable = true;
      dawnTime = "6:00-8:00";
      duskTime = "20:00-22:00";
      provider = "geoclue2";
      temperature.day = 6000;
      temperature.night = 3500;
    };

    kanshi = {
      # autodetect and arrange external monitors
      enable = true;
      systemdTarget = "sway-session.target";
      settings =
        let
          pactl = "${pkgs.pulseaudio}/bin/pactl";
          wpctl = "${pkgs.wireplumber}/bin/wpctl";
          pwcli = "${pkgs.pipewire}/bin/pw-cli";
          sm = "${pkgs.sway}/bin/swaymsg";
          mwss =
            o:
            map (ws: ''
              ${sm} "workspace --no-auto-back-and-forth ${ws}, move workspace to output '${o}'"
            '');
        in
        [
          {
            profile.name = "undocked";
            profile.outputs = [ { criteria = "${laptop.screen}"; } ];
            profile.exec = [
              "${pactl} set-default-sink ${laptop.sink}"
              "${pactl} set-default-source ${laptop.source}"
              # "${pactl} set-card-profile ${laptop.card} "${laptop.profile} '"

              # "${wpctl} set-default 129" # audio sink = headphones
              # "${wpctl} set-default 46"  # audio source = logitech mic
              # "${wpctl} set-default 115" # video source = logitech camera

              # # device = 51 is standard audio controller
              # # profile index = 1 is headphones
              # # profile index = 2 is speaker
              # pw-cli s 51 Profile '{index:2, save:true}'
              # pw-cli s 51 Profile '{index:1, save:true}'

            ];
          }

          {
            profile.name = "wgb";
            profile.outputs = [
              {
                criteria = "${laptop.screen}"; # 3840x2400
                position = "0,2160"; # below ${hdmi} => hdmi_y = 2160 / 1.0
                scale = 2.0;
              }
              {
                criteria = "${wgb.screen}"; # 3840x2160
                position = "640,0"; # overlap right-third => laptop_x => 3840 / 2.0 * (2/3)
                scale = 1.0;
              }
            ];
            profile.exec = [
              "${pactl} set-default-sink ${wgb.sink}"
              "${pactl} set-default-source ${wgb.source}"
            ]
            ++ (mwss wgb.screen [
              homews
              codews
              chatws
            ])
            ++ (mwss wgb.screen otherws)
            ++ (mwss laptop.screen [
              mailws
              mediaws
            ])
            ++ [ ''${sm} "workspace --no-auto-back-and-forth 1"'' ];
          }

          {
            profile.name = "christs";
            profile.outputs = [
              {
                criteria = "${laptop.screen}"; # 3840x2400
                position = "0,2160"; # below ${hdmi} => hdmi_y = 2160 / 1.0
                scale = 2.0;
              }
              {
                criteria = "${christs.screen}"; # 3840x2160
                position = "640,0"; # overlap right-third => laptop_x => 3840 / 2.0 * (2/3)
                scale = 1.0;
              }
            ];
            profile.exec = [
              "${pactl} set-default-sink ${laptop.sink}"
              "${pactl} set-default-source ${christs.source}"
              # "${pactl} set-card-profile ${laptop.card} '${laptop.profile}'"
            ]
            ++ (mwss christs.screen [
              homews
              codews
              chatws
            ])
            ++ (mwss christs.screen otherws)
            ++ (mwss laptop.screen [
              mailws
              mediaws
            ])
            ++ [ ''${sm} "workspace --no-auto-back-and-forth 1"'' ];
          }

          {
            profile.name = "tv";
            profile.outputs = [
              {
                criteria = "${laptop.screen}"; # 3840x2400
                position = "0,0";
                scale = 2.0;
              }
              {
                criteria = "${tv.screen}"; # 3840x2160
                position = "1920,60"; # adjacent, right-hand side, vertically centred
                scale = 1.0;
              }
            ];
            profile.exec = [
              "${pactl} set-default-sink ${tv.sink}"
            ]
            ++ (mwss tv.screen [ "10" ])
            ++ [ ''${sm} "workspace --no-auto-back-and-forth 10"'' ];
          }
        ];
    };

    swayidle =
      # screen saving and locking
      let
        lock = "${pkgs.swaylock}/bin/swaylock -C ~/.config/swaylock/config";
        suspend = "${pkgs.systemd}/bin/systemctl suspend";
      in
      {
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
        events = {
          "before-sleep" = "${lock}";
          "lock" = "${lock}";
        };
      };

    swayosd.enable = true;
  };

  programs = {

    i3status-rust = {
      # providing information for status bars
      enable = true;
      bars = {
        top = {
          theme = "solarized-dark";
          icons = "emoji";
          blocks =
            let
              caffeine_on = {
                icon = "pomodoro_break";
                text = "";
              };
              caffeine_off = {
                icon = "resolution";
                text = "";
              };
              caffeine = pkgs.writeShellScriptBin "caffeine.sh" ''
                inhibitor_count () {
                  local tree=$(swaymsg -t get_tree -r)
                  printf "%s" $tree | jq --stream -c . | rg 'inhibit_idle"],true' | wc -l
                }

                off () {
                  swaymsg -q 'for_window [all] inhibit_idle none' &&
                    swaymsg -q '[all] inhibit_idle none' &&
                    printf '${builtins.toJSON caffeine_off}'
                }

                on () {
                  swaymsg -q 'for_window [all] inhibit_idle open' &&
                    swaymsg -q '[all] inhibit_idle open' &&
                    printf '${builtins.toJSON caffeine_on}' $(inhibitor_count)
                }

                case $1 in
                  off ) off ;;
                  on ) on ;;
                  ic ) inhibitor_count ;;
                  * )
                    printf "invalid parameter '$1'\n" >/dev/stderr
                    exit 1
                    ;;
                esac
              '';
            in
            [
              {
                block = "battery";
                format = " $icon $percentage {$time |}";
                charging_format = " $icon $percentage ";
                full_format = " $icon $percentage {$time |}";
              }
              {
                block = "memory";
                # format = "$icon$mem_used_percents [$swap_used_percents]"; # XXX swap_used_percents returning NaN fails to render
                format = " $icon$mem_used.eng(prefix:Mi)/$mem_total.eng(prefix:Mi)($mem_used_percents.eng(w:2))";
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
                command = "${caffeine}/bin/caffeine.sh off";
                cycle = [
                  "${caffeine}/bin/caffeine.sh on"
                  "${caffeine}/bin/caffeine.sh off"
                ];
              }
              {
                block = "time";
                format = "$timestamp.datetime(f:'%c %Z')";
                interval = 1;
              }
            ];
        };

        bottom = {
          theme = "solarized-dark";
          icons = "emoji";
          blocks = [
            {
              block = "net";
              format = " $icon $device {$ip|NO IPv4} {$ipv6|NO IPv6} {($signal_strength $ssid)|}";
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

}
