case $(uname -s) in
  Darwin ) ## likely to be my (new) laptop
    brew install stow
    ;;

  Linux )
    if [ ! -d ~/.nix-profile ]; then
      sudo apt install -yy curl stow || true
    fi

    ;;
esac

APPS="ocaml pandoc python"
stow --dotfiles \
     bash \
     emacs \
     git \
     indent \
     screen \
     ssh \
     tmux \
     wget

case $(uname -s) in
  Darwin ) ## likely to be my (new) laptop
    stow --dotfiles $APPS
    launchctl load ~/Library/LaunchAgents/homebrew.mxcl.offlineimap.plist
    ;;

  Linux ) ## likely to be a random server
    if [ -d ~/.nix-profile ]; then ## ...or a linux laptop?! :)
      stow --dotfiles $APPS
    else
      ## many linux distros appear to have old git-prompt.sh
      GITHUB=https://raw.githubusercontent.com
      curl $GITHUB/git/git/master/contrib/completion/git-prompt.sh \
           -o ~/.git-prompt.sh
    fi
    ;;
esac
