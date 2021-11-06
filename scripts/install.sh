case $(uname -s) in
  Darwin ) ## likely to be my (new) laptop
    brew install stow
    ;;

  Linux )
    sudo apt install -yy curl stow
    ;;
esac

stow --dotfiles \
     bash \
     emacs \
     git \
     indent \
     screen \
     tmux \
     wget

case $(uname -s) in
  Darwin ) ## likely to be my (new) laptop
    stow --dotfiles \
         karabiner \
         local \
         ocaml \
         offlineimap \
         pandoc \
         python

    launchctl load ~/Library/LaunchAgents/homebrew.mxcl.offlineimap.plist
    ;;

  Linux ) ## likely to be a random server
    ## many linux distros appear to have old git-prompt.sh
    GITHUB=https://raw.githubusercontent.com
    curl $GITHUB/git/git/master/contrib/completion/git-prompt.sh \
         -o ~/.git-prompt.sh
    ;;
esac
