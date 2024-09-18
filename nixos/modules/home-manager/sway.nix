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
  laptop = {
    screen = "eDP-1";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Speaker__sink";
    source = "alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Mic1__source";
  };
  wgb = {
    screen = "LG Electronics LG HDR 4K 0x0005DD99";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__HDMI1__sink";
    source = "alsa_input.usb-046d_HD_Pro_Webcam_C920_C18974EF-02.analog-stereo";
  };
  o2 = {
    screen = "LG Electronics LG HDR 4K 0x00035DAC";
    source = "alsa_input.usb-046d_0990_F6BD69E7-02.pro-input-0";
  };
  tv = {
    screen = "Panasonic Industry Company Panasonic-TV 0x01010101";
    sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__HDMI1__sink";
  };
in
# nms_a = {
#   screen = "HDMI-A-1";
# };
{

  home.packages = with pkgs; [
    brightnessctl # control screen brightness
    gammastep # automatically dim+redden screen at night
    grim
    kanshi # modify sway config on hardware changes
    pwvucontrol # graphical control of AV routing (pipewire)
    slurp
    wdisplays # gui for display configuration
    wev # wayland event viewer
    wl-clipboard # pipe to/from clipboard
    wlclock # old skool analogue
    xorg.xset # used by vlc via xdg-screensaver to manage screensaver timeouts
  ];

  wayland.windowManager.sway.checkConfig = false;

  wayland.windowManager.sway = {
    enable = true;

    swaynag.enable = true;

    wrapperFeatures.gtk = true;

    config =
      let
        swayosd = lib.getExe' pkgs.swayosd "swayosd-client";
      in
      {
        modifier = "Mod4"; # use WIN not ALT-L for sway controls
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
              wait_for codium
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
              wait_for skypeforlinux
              ${after 3 [
                "[class=Skype] focus"
                "split vertical"
              ]}
              # some signal weirdness prevents the window appearing until a second
              # copy is run, and immediately exits on detecting it's the second instance
              wait_for "signal-desktop & sleep 3 && signal-desktop"
              ${after 1 [ "[class=Skype] layout stacking" ]}

              ${workspace "${homews}"}
              wait_for emacsclient -c -s /tmp/emacs-mort/server
              ${after 1 [ "split horizontal" ]}
              wait_for foot
              ${after 1 [ "split vertical" ]}
              wait_for firefox -P default

              ${after 1 [
                "reload"
                "exec systemctl restart --user kanshi.service"
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
            tap = "enabled";
            tap_button_map = "lmr";
          };
          "pointer" = {
            accel_profile = "adaptive";
          };
        };

        output = {
          "*" = {
            bg = "${background} fill";
          };
        };

        seat = {
          "*" = {
            xcursor_theme = "Bibata-Modern-Ice 18";
          };
        };

        # additional keybindings; cannot simply remap input ev -> output ev
        keybindings =
          let
            f1 = "exec ${swayosd} --max-volume 130 --output-volume mute-toggle";
            f2 = "exec ${swayosd} --max-volume 130 --output-volume lower";
            f3 = "exec ${swayosd} --max-volume 130 --output-volume raise";
            f4 = "exec ${swayosd} --input-volume mute-toggle";
            f5 = "exec brightnessctl s 10%-";
            f6 = "exec brightnessctl s 10%+";
            f7 = "nop f7 pressed";
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
            ## thinkpad keyboard
            "XF86AudioMute" = f1;
            "XF86AudioLowerVolume" = f2;
            "XF86AudioRaiseVolume" = f3;
            "XF86AudioMicMute" = f4;
            "XF86MonBrightnessDown" = f5;
            "XF86MonBrightnessUp" = f6;
            "XF86Display" = f7;
            "XF86WLAN" = f8;
            "XF86Messenger" = f9;
            "XF86Go" = f10;
            "Cancel" = f11;
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
            # F10 !!! XXX NO SCAN CODE
            "XF86Save" = f11;
            "Print" = f12; # also catches PrtSc on thinkpad

            ## extras, all keyboards
            # "Mod4+Return" = "exec ${pkgs.rio}/bin/rio";
          };

        # status bars using i3status-rust
        bars =
          let
            status = "${pkgs.i3status-rust}/bin/i3status-rs";
          in
          [
            {
              position = "top";
              statusCommand = "${status} ~/.config/i3status-rust/config-top.toml";
            }
            {
              position = "bottom";
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
            profile.exec =
              [
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
            profile.name = "o2";
            profile.outputs = [
              {
                criteria = "${laptop.screen}"; # 3840x2400
                position = "0,2160"; # below ${hdmi} => hdmi_y = 2160 / 1.0
                scale = 2.0;
              }
              {
                criteria = "${o2.screen}"; # 3840x2160
                position = "640,0"; # overlap right-third => laptop_x => 3840 / 2.0 * (2/3)
                scale = 1.0;
              }
            ];
            profile.exec =
              [
                "${pactl} set-default-sink ${laptop.sink}"
                "${pactl} set-default-source ${o2.source}"
              ]
              ++ (mwss o2.screen [
                homews
                codews
                chatws
              ])
              ++ (mwss o2.screen otherws)
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
            ] ++ (mwss tv.screen [ "10" ]) ++ [ ''${sm} "workspace --no-auto-back-and-forth 10"'' ];
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
                text = "<%d>";
              };
              caffeine_off = {
                icon = "resolution";
                text = "";
              };
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
                  swaymsg -q 'for_window [all] inhibit_idle none' &&
                    swaymsg -q '[all] inhibit_idle none' &&
                    printf '${builtins.toJSON caffeine_off}'
                '';
                cycle =
                  let
                    inhibitor_count = ''
                      $(swaymsg -t get_tree -r | jq --stream -c . | rg 'inhibit_idle"],true' | wc -l)
                    '';
                  in
                  [
                    ''
                      swaymsg -q 'for_window [all] inhibit_idle none' &&
                        swaymsg -q '[all] inhibit_idle none' &&
                        printf '${builtins.toJSON caffeine_off}'
                    ''
                    ''
                      swaymsg -q 'for_window [all] inhibit_idle open' &&
                        swaymsg -q '[all] inhibit_idle open' &&
                        printf '${builtins.toJSON caffeine_on}' ${inhibitor_count}
                    ''
                  ];
              }
              {
                block = "time";
                format = "$timestamp.datetime(f:'%c')";
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

  home.pointerCursor = {
    name = "Bibata-Modern-Ice";
    package = pkgs.bibata-cursors;
    size = 20;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "wayland-cursor";
    };
  };
}
