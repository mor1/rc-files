{ lib, config, nixpkgs, pkgs, ... }:

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
    builtins.elem (lib.getName pkg) [
      "corefonts"
      "slack"
      "skypeforlinux"
      "teams"
      "vista-fonts" # vista-fonts here but vistafonts for install?!
      "zoom" # zoom here but zoom-us for install?!
    ];

  home.packages = with pkgs;
    let
      system = [
        (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
        davmail
        direnv
        gnupg
        hunspell # spellchecking and dictionaries
        hunspellDicts.en_GB-large
        kbfs
        keybase
        krb5
        maestral
        onedrive
        pciutils
        sshfs
        stow
        strongswan
        usbutils
      ];

      apps = (let
        sway_apps = [
          brightnessctl # control screen brightness
          gammastep # automatically dim+redden screen at night
          grim
          kanshi # modify sway config on hardware changes
          slurp
          swayosd # on-screen display for various states
          wdisplays # gui for display configuration
          wev # wayland event viewer
          wl-clipboard # pipe to/from clipboard
          wlclock # old skool analogue
        ];
        cli_apps = [
          bc # calculator
          dig # because DNS
          exiftool # manipulate images
          file # identify filetype by magic
          get_iplayer # download from iPlayer
          ghostscript # ps etc
          handlr # manage XDG Open mappings
          pandoc # document processing and conversion
          imagemagick # image manipulation tools
          inetutils
          jhead # jpeg exif header manipulation tool
          keychain # cli to manage SSH, GPG keys
          lynx # cli web browser
          mupdf # PDF manipulation
          pdftk # more PDF manipulation
          qpdf # yet more PDF manipulation
          subversion # hysterical raisins
          sysstat # system stats
          texlive.combined.scheme-full # latex installation
          traceroute
          tree # tree-format recursive ls
          typst # better latex?
          unzip # what it says on the tin, because zip can't unzip
          wget # network downloader
          which # locate command in $PATH
          zip # what it says on the tin
        ];
        gui_apps = [
          alacritty # alternative xterm
          chromium # teams calling in browser doesn't work in firefox
          firefox # web browser
          gnome.nautilus # maybe the least sucky of the file managers, so far?
          inkscape # vector graphics editing
          keybase-gui # keybase
          libreoffice # ~ms office
          meld # compare files / folders
          networkmanagerapplet # nm-connection-manager, NetworkManager GUI
          okular # pdf viewer / annotator
          signal-desktop # signal private messaging
          skypeforlinux # skype
          slack # slack
          teams-for-linux # ms teams in electron
          thunderbird # email
          wire-desktop # wire private messaging
          zoom-us # zoom vc
        ];
        media_apps = [ digikam greg kodi rhythmbox vlc ];
        nu_posix = [
          bat # better cat
          bottom # btm ~ better top, htop, etc
          broot # interactive directory navigation
          delta # better syntax highlighting diff
          dua # disk usage, interactively
          eza # improved `ls`
          fd # `find` replacement
          fzf # fuzzy file finder; desired by yazi
          hexyl # hex pretty printer
          htop # graphical top
          jujutsu # better git
          just # updated gnumake replacement
          mcfly # better shell history
          nushell # maybe it's time to kick another addiction
          procs # better ps
          ripgrep # rg ~ `grep` replacement
          uutils-coreutils-noprefix # replaces GNU `coreutils`
          viddy # better watch
          yazi # file manager
          zoxide # smarter cd; desired by yazi
        ];
      in sway_apps ++ cli_apps ++ gui_apps ++ media_apps ++ nu_posix);

      fonts = [
        (nerdfonts.override { fonts = [ "Hack" ]; })
        corefonts
        # font-awesome_4
        # material-design-icons
        # powerline-fonts
        vistafonts
      ];

      dev_tools = (let
        python_tools = [ hatch python312 ruff uv ]
          ++ (with python312Packages; [ autopep8 pip pygments ]);
        ocaml_tools = [ gcc ocaml dune_3 ocamlformat opam ]
          ++ (with ocamlPackages; [
            cmdliner
            findlib
            merlin
            ocaml-lsp
            ocp-indent
            utop
          ]);
      in [
        emacs29 # even after all, because org-mode
        gh # github CLI
        git # obviously
        git-filter-repo # for fixing up repos
        git-lfs # large file support
        gnumake # unavoidably
        jq # pretty-print JSON
        nil # LSP for Nix language
        nixfmt # format .nix files
        rustup # manage Rust installations
      ] ++ python_tools ++ ocaml_tools);

      # brave evolution evolution-ews mailspring
    in system ++ apps ++ dev_tools ++ fonts;

  fonts.fontconfig.enable = true;

  wayland.windowManager.sway = {
    enable = true;

    swaynag.enable = true;

    wrapperFeatures.gtk = true;

    config = let swayosd = lib.getExe' pkgs.swayosd "swayosd-client";
    in rec {
      modifier = "Mod4"; # use WIN not ALT-L for sway controls
      focus.wrapping = "force";
      workspaceAutoBackAndForth = true;

      # build the startup script to start apps in workspaces
      startup = let
        msg = cmds: "swaymsg '${builtins.concatStringsSep ", " cmds}'";
        workspace = ws: msg [ "workspace --no-auto-back-and-forth ${ws}" ];
        after = delay: cmds: "sleep ${toString delay} && ${msg cmds}";
        now = after 0;
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

          ${msg [ "exec ${swayosd} --max-volume 160" ]}

          ${workspace "5:media"}
          wait_for "rhythmbox"

          ${workspace "4:chat"}
          wait_for slack
          ${after 1 [ "split horizontal" ]}
          wait_for skypeforlinux
          ${after 3 [ "[class=Skype] focus" "split vertical" ]}
          # some signal weirdness prevents the window appearing until a second
          # copy is run, and fails to start...
          wait_for "signal-desktop & sleep 2 && signal-desktop"
          ${after 1 [ "[class=Skype] layout stacking" ]}

          ${workspace "3:mail"}
          wait_for firefox -P richard.mortier@gmail.com
          wait_for firefox -P 14mortier@gmail.com
          wait_for firefox -P mort@ikva.ai
          wait_for firefox -P rmm1002@cam.ac.uk
          wait_for teams-for-linux
          ${after 3 [ "layout stacking" ]}

          ${workspace "2:code"}
          wait_for firefox -P github.com
          wait_for codium
          ${after 3 [ "layout stacking" ]}

          ${workspace "1"}
          wait_for emacsclient -c -s /tmp/emacs-mort/server
          ${after 1 [ "split horizontal" ]}
          wait_for foot
          ${after 1 [ "split vertical" ]}
          wait_for firefox -P default

          ${after 1 [ "reload" "kanshictl reload" ]}
        '';
      in [{ command = "${startup}/bin/startup.sh"; }];

      # all my keyboards are GB layouta
      input = {
        "*" = { xkb_layout = "gb"; };
        "type:touchpad" = {
          natural_scroll = "enabled";
          tap = "enabled";
          tap_button_map = "lmr";
        };
        "pointer" = { accel_profile = "adaptive"; };
      };

      # additional keybindings; cannot simply remap input ev -> output ev
      keybindings = let
        wlandev = "wlp0s20f3";
        netdevs = [
          "enp0s20f0u6u3c2" # wired, O2
          "enp0s13f0u3u1" # wired, GB
          "${wlandev}" # wireless
        ];
        f1 = "exec ${swayosd} --output-volume mute-toggle";
        f2 = "exec ${swayosd} --output-volume lower";
        f3 = "exec ${swayosd} --output-volume raise";
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
        f8 = "exec wifi-toggle";
        f9 = "exec rhythmbox-client --play-pause";
        f10 = "exec rhythmbox-client --stop";
        f11 = "exec rhythmbox-client --previous";
        f12 = "exec rhythmbox-client --next";
      in lib.mkOptionDefault {
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
        "Print" = f12;
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
    extraConfig = ''
      output "*" bg ${background} fill
    '';
  };

  systemd.user.services.kbfs.Service.PrivateTmp = lib.mkForce false;

  services = {

    emacs = {
      # until the addiction is kicked
      package = pkgs.emacs29;
      enable = true;
      client.enable = true;
      # defaultEditor = true;
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
        laptop = {
          screen = "eDP-1";
          sink =
            "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp__sink";
        };
        wgb = {
          screen = "LG Electronics LG HDR 4K 0x0005DD99";
          sink =
            "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_3__sink";
        };
        o2 = {
          screen = "LG Electronics LG HDR 4K 0x00035DAC";
          sink =
            "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp__sink";
        };
        # nms_a = {
        #   screen = "HDMI-A-1";
        # };
        pactl = "${pkgs.pulseaudio}/bin/pactl";
        sm = "${pkgs.sway}/bin/swaymsg";
        mws = w: o: ''
          ${sm} "workspace --no-auto-back-and-forth ${w}, move workspace to output '${o}'"
        '';
      in {
        undocked = {
          outputs = [{ criteria = "${laptop.screen}"; }];
          exec = [ "${pactl} set-default-sink ${laptop.sink}" ];
        };
        wgb = {
          outputs = [
            {
              criteria = "${laptop.screen}"; # 3840x2400
              position = "0,2160"; # below ${hdmi} => hdmi_y = 2160 / 1.0
              scale = 2.0;
            }
            {
              criteria = "${wgb.screen}"; # 3840x2160
              position =
                "640,0"; # overlap right-third => laptop_x => 3840 / 2.0 * (2/3)
              scale = 1.0;
            }
          ];
          exec = [
            "${mws "1" wgb.screen}"
            "${mws "2:code" wgb.screen}"
            "${mws "3:mail" laptop.screen}"
            "${mws "4:chat" wgb.screen}"
            "${mws "5:media" laptop.screen}"
            "${pactl} set-default-sink ${wgb.sink}"
            ''${sm} "workspace --no-auto-back-and-forth 1"''
          ];
        };
        o2 = {
          outputs = [
            {
              criteria = "${laptop.screen}"; # 3840x2400
              position = "0,2160"; # below ${hdmi} => hdmi_y = 2160 / 1.0
              scale = 2.0;
            }
            {
              criteria = "${o2.screen}"; # 3840x2160
              position =
                "640,0"; # overlap right-third => laptop_x => 3840 / 2.0 * (2/3)
              scale = 1.0;
            }
          ];
          exec = [
            "${mws "1" o2.screen}"
            "${mws "2:code" o2.screen}"
            "${mws "3:mail" laptop.screen}"
            "${mws "4:chat" o2.screen}"
            "${pactl} set-default-sink ${o2.sink}"
            "${mws "5:media" laptop.screen}"
            ''${sm} "workspace --no-auto-back-and-forth 1"''
          ];
        };
        # o2-closed = {
        #   outputs = [{
        #     criteria = "${o2.screen}"; # 3840x2160
        #     position =
        #       "640,0"; # overlap right-third => laptop_x => 3840 / 2.0 * (2/3)
        #     scale = 1.0;
        #   }];
        #   exec = [
        #     "${mws "3:mail" o2.screen}"
        #     "${mws "5:media" o2.screen}"
        #   ];
        # };
      };
    };

    keybase.enable = true;
    kbfs = {
      enable = true;
      # enableRedirector = true;
    };

    swayidle =
      # screen saving and locking
      let
        lock = "${pkgs.swaylock}/bin/swaylock -C ~/.config/swaylock/config";
        suspend = "${pkgs.systemd}/bin/systemctl suspend";
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

    swayosd.enable = true;
  };

  programs = {

    bash = {
      enable = false;
      enableCompletion = false;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    chromium = { enable = true; };

    firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
      # profiles.default.extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      #   privacy-badger
      #   https-everywhere
      # ];
    };

    git = { enable = true; };

    i3status-rust = {
      # providing information for status bars
      enable = true;
      bars = {
        top = {
          theme = "solarized-dark";
          icons = "emoji";
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
                  swaymsg -q inhibit_idle open && printf '${
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
          icons = "emoji";
          blocks = [
            {
              block = "net";
              format =
                " $icon $device {$ip|NO IPv4} {$ipv6|NO IPv6} {($signal_strength $ssid)|}";
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

    keychain = {
      enable = true;
      agents = [ "gpg" "ssh" ];
    };

    opam = {
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

    vscode = {
      enable = true;

      package = pkgs.vscodium.fhsWithPackages
        (ps: with ps; [ rustup zlib openssl.dev pkg-config ]);

      extensions = with pkgs.vscode-extensions;
        [
          arrterian.nix-env-selector
          ban.spellright
          bbenoist.nix
          betterthantomorrow.calva
          charliermarsh.ruff
          foxundermoon.shell-format
          jnoortheen.nix-ide
          kahole.magit
          ms-pyright.pyright
          ms-python.python
          ocamllabs.ocaml-platform
          rust-lang.rust-analyzer
          shd101wyy.markdown-preview-enhanced
          stkb.rewrap
          tamasfe.even-better-toml
          tuttieee.emacs-mcx
          usernamehw.errorlens
          yzhang.markdown-all-in-one
        ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
          publisher = "bierner";
          name = "markdown-preview-github-styles";
          version = "2.0.4";
          sha256 = "sha256-jJulxvjMNsqQqmsb5szQIAUuLWuHw824Caa0KArjUVw=";
        }];

      userSettings = {
        "editor.fontFamily" = "Hack";
        "editor.fontSize" = 11;
        "editor.indentSize" = "tabSize";
        "editor.multiCursorModifier" = "ctrlCmd";
        "editor.renderWhitespace" = "none";
        "editor.rulers" = [
          {
            "column" = 72;
            "color" = "#aaa";
          }
          {
            "column" = 100;
            "color" = "#aaa";
          }
          { # alpha=0 ~ transparent
            "column" = 0;
            "color" = "#0000";
          }
        ];
        "editor.useTabStops" = false;
        # "editor.wordWrap" = "bounded";

        "explorer.confirmDelete" = false;

        "files.autoSave" = "afterDelay";
        "files.autoSaveDelay" = 2000;
        "files.insertFinalNewline" = true;
        "files.trimFinalNewlines" = true;

        "git.allowForcePush" = true;
        "git.confirmSync" = false;

        "github.gitProtocol" = "ssh";

        "hungryDelete.considerIncreaseIndentPattern" = true;
        "hungryDelete.followAboveLineIndent" = true;

        "interactiveSession.editor.fontSize" = 11;

        "nix.enableLanguageServer" = true;
        "nix.formatterPath" = "nixfmt";
        "nix.serverPath" = "nil";
        "nix.serverSettings" = { };

        "rewrap.autoWrap.enabled" = true;

        "security.workspace.trust.untrustedFiles" = "open";

        "spellright.language" = [ "English (British)" ];

        "terminal.integrated.fontSize" = 11;
        "terminal.integrated.sendKeybindingsToShell" = true;

        "workbench.colorTheme" = "Solarized Dark";
        "workbench.preferredDarkColorTheme" = "Solarized Dark";
        "workbench.preferredLightColorTheme" = "Solarized Light";

        "[markdown]" = {
          "preview.typographer" = true;
          "diffEditor.ignoreTrimWhitespace" = false;
          "editor.quickSuggestions" = {
            "comments" = "off";
            "strings" = "off";
            "other" = "off";
          };
          "editor.rulers" = [
            {
              "column" = 80;
              "color" = "#aaa";
            } # alpha=0 ~ transparent
            {
              "column" = 0;
              "color" = "#0000";
            } # alpha=0 ~ transparent

          ];
          "editor.unicodeHighlight.ambiguousCharacters" = false;
          "editor.unicodeHighlight.invisibleCharacters" = false;
          # "editor.wordWrap" = "on";
        };

        "[python]" = {
          "editor.formatOnSave" = true;
          "editor.codeActionsOnSave" = {
            "source.fixAll" = "explicit";
            "source.organizeImports" = "explicit";
          };
          "editor.defaultFormatter" = "charliermarsh.ruff";
        };

      };

      keybindings = [
        # (bind-keys*
        #   ("%"          . match-paren)
        #   ("C-<tab>"    . dabbrev-expand)

        # {
        #   key = "ctrl+c";
        #   command = "workbench.action.terminal.sendSequence";
        #   args = { text = "u0003"; };
        #   when =
        #     "terminalFocus && terminalHasBeenCreated || terminalFocus && terminalProcessSupported";
        # }

        {
          key = "ctrl+c t";
          command = "workbench.action.toggleLightDarkThemes";
        }

        {
          key = "ctrl+x g";
          command = "magit.status";
        }

        {
          key = "ctrl+enter";
          command = "runCommands";
          args = {
            commands = [
              { command = "emacs-mcx.newLine"; }
              {
                command = "cursorMove";
                args = {
                  to = "up";
                  by = "line";
                };
              }
              {
                command = "cursorMove";
                args = { to = "wrappedLineLastNonWhitespaceCharacter"; };
              }
            ];
            when = "textInputFocus";
          };
        }

        {
          key = "ctrl+c ctrl+space";
          command = "runCommands";
          args = {
            commands = [
              {
                command = "remove-empty-lines.inDocument";
                args = 2;
              }
              { command = "editor.action.trimTrailingWhitespace"; }
              { command = "editor.action.indentationToSpaces"; }
            ];
          };
          when = "textInputFocus";
        }

        {
          key = "alt+n";
          command = "workbench.action.nextEditor";
        }
        {
          key = "alt+p";
          command = "workbench.action.previousEditor";
        }

        {
          key = "ctrl+c ;";
          command = "editor.action.addCommentLine";
          when = "textInputFocus";
        }
        {
          key = "ctrl+u ctrl+c ;";
          command = "editor.action.removeCommentLine";
          when = "textInputFocus";
        }

        {
          key = "ctrl+pageup";
          command = "cursorMove";
          args = { to = "viewPortTop"; };
          when = "textInputFocus";
        }
        {
          key = "ctrl+pagedown";
          command = "runCommands";
          args = {
            # viewPortBottom alone causes a scroll up by one...
            commands = [
              {
                command = "editorScroll";
                args = {
                  to = "up";
                  by = "line";
                };
              }
              {
                command = "cursorMove";
                args = { to = "viewPortBottom"; };
              }
            ];
          };
          when = "textInputFocus";
        }
      ];
    };

  };

  home.stateVersion = "24.05";
}
