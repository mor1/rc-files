{ pkgs, lib, ... }:
{

  programs.vscode = {
    enable = true;

    package = pkgs.vscodium.fhsWithPackages (
      ps: with ps; [
        rustup
        zlib
        openssl.dev
        pkg-config
      ]
    );

    extensions =
      with pkgs.vscode-extensions;
      [
        arrterian.nix-env-selector
        ban.spellright
        bbenoist.nix
        bodil.file-browser
        charliermarsh.ruff
        ecmel.vscode-html-css
        foxundermoon.shell-format
        james-yu.latex-workshop
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
      ]
      ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          publisher = "bierner";
          name = "markdown-preview-github-styles";
          version = "2.0.4";
          sha256 = "sha256-jJulxvjMNsqQqmsb5szQIAUuLWuHw824Caa0KArjUVw=";
        }
        {
          publisher = "karunamurti";
          name = "tera";
          version = "0.0.9";
          sha256 = "sha256-e72lZXg//vCZwoggRrpJlYiNUMxID3rkDLLBtV1b098=";
        }
        {
          publisher = "kokakiwi";
          name = "vscode-just";
          version = "2.1.0";
          sha256 = "sha256-1ncWDFG111HJ+PmA6k011qgC4uWMOs/wiE4F0A48UtY=";
        }
      ];

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
        {
          # alpha=0 ~ transparent
          "column" = 0;
          "color" = "#0000";
        }
      ];
      "editor.useTabStops" = false;
      "explorer.confirmDelete" = false;
      "files.autoSave" = "afterDelay";
      "files.autoSaveDelay" = 2000;
      "files.insertFinalNewline" = true;
      "files.trimFinalNewlines" = true;
      "git.allowForcePush" = true;
      "git.autofetch" = true;
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
      "terminal.integrated.allowChords" = false;
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

      # zoom in/out/reset but only for the editor
      {
        key = "ctrl+x ctrl+=";
        command = "runCommands";
        args = {
          commands = [
            { command = "editor.action.fontZoomIn"; }
            { command = "workbench.action.terminal.fontZoomIn"; }
          ];
        };
      }
      {
        key = "ctrl+x ctrl+-";
        command = "runCommands";
        args = {
          commands = [
            { command = "editor.action.fontZoomOut"; }
            { command = "workbench.action.terminal.fontZoomOut"; }
          ];
        };
      }
      {
        key = "ctrl+x ctrl+0";
        command = "runCommands";
        args = {
          commands = [
            { command = "editor.action.fontZoomReset"; }
            { command = "workbench.action.terminal.fontZoomReset"; }
          ];
        };
      }

      {
        key = "ctrl+x ctrl+f";
        command = "file-browser.open";
      }

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
              args = {
                to = "wrappedLineLastNonWhitespaceCharacter";
              };
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
        args = {
          to = "viewPortTop";
        };
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
              args = {
                to = "viewPortBottom";
              };
            }
          ];
        };
        when = "textInputFocus";
      }
    ];
  };
}
