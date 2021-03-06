#!/usr/bin/env bash
#
# Copyright (C) 2000--2015 Richard Mortier <mort@cantab.net>.  All Rights
# Reserved.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as
# published by the Free Software Foundation
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

# NB. this script assumes it is run from the directory of the rc-files repo

set -ex
READLINK=readlink
if [ "$(uname -s)" = "Darwin" ]; then
  READLINK=greadlink
fi

INDIR=$(dirname "$($READLINK -f "$0")") # cd to script directory
pushd $INDIR

## capture any existing SSH state
[ ! -L ~/.ssh ] \
    && for file in ~/.ssh/{authorized_keys,known_hosts,*-key,*-key.pub} ; do

    [ -s $file ] && mv $file ssh
done
[ -d ~/.ssh -a ! -L ~/.ssh ] \
    && ( rmdir ~/.ssh ; ln -s $INDIR/ssh ~/.ssh )

case $(uname -s) in

    Darwin ) ## likely to be my (new) laptop
        TARGETS="
          Xresources
          bash_*
          emacs.d
          environment
          envrc
          gitconfig
          gitlocal
          indent.pro
          iocamlinit
          karabiner.xml
          ocamlinit
          offlineimap*
          pandoc
          pythonrc
          screenrc
          vimrc
          wgetrc
          "
        ## install any launchers
        ln -sfv ~/rc-files/*.plist ~/Library/LaunchAgents

        ## offlineimap
        brew install offlineimap
        mkdir -p ~/Library/LaunchAgents
        launchctl load ~/Library/LaunchAgents/homebrew.mxcl.offlineimap.plist
        ;;

    Linux ) ## likely to be a random server
        TARGETS="bash_* environment gitconfig gitlocal"

        ## many linux distros appear to have old git-prompt.sh which breaks
        ## things
        GITHUB=https://raw.githubusercontent.com
        curl $GITHUB/git/git/master/contrib/completion/git-prompt.sh \
             -o ~/.git-prompt.sh
        ;;
esac

for f in $TARGETS; do
    [ -L ~/.$f ] && rm -f ~/.$f || true
    ln -s $INDIR/$f ~/.$f
done

## i value consistency in my environments. so what?
rm -f ~/.bashrc
ln -s $INDIR/bash_profile ~/.bashrc

popd
