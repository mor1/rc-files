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
INDIR=$(dirname "$(readlink -f "$0")") # cd to script directory
pushd $INDIR

## capture any existing SSH state
[ ! -L ~/.ssh ] \
    && for file in ~/.ssh/{authorized_keys,known_hosts,*-key,*-key.pub} ; do

    [ -s $file ] && mv $file ssh
done
[ -d ~/.ssh ] && rmdir ~/.ssh
ln -s $INDIR/ssh ~/.ssh

## i value consistency in my environments. so what?
rm -f ~/.bashrc
ln -s $INDIR/bash_profile ~/.bashrc

case $(uname -s) in

    Darwin ) ## likely to be my (new) laptop
        TARGETS="bash_* colours.sh envfns.sh filefns.sh hosts.sh environment \
                 gitconfig gitlocal indent.pro iocamlinit ocamlinit pandoc \
                 pythonrc screenrc vimrc wgetrc Xresources floatlg.jpg \
                 solarized-* karabiner.xml offlineimap-* offlineimap.py* \
                 emacs.d \
                 "
        ## install any launchers
        ln -sfv ~/rc-files/*.plist ~/Library/LaunchAgents

        ## offlineimap
        brew install offlineimap
        mkdir -p ~/Library/LaunchAgents
        launchctl load ~/Library/LaunchAgents/homebrew.mxcl.offlineimap.plist
        ;;

    Linux ) ## likely to be a random server
        TARGETS="bash_* colours.sh envfns.sh filefns.sh hosts.sh environment \
                 gitconfig gitlocal \
                 "
        ## many linux distros appear to have old git-prompt.sh which breaks
        ## things
        GITHUB=https://raw.githubusercontent.com
        curl $GITHUB/git/git/master/contrib/completion/git-prompt.sh \
             -o ~/.git-prompt.sh
        ;;
esac

for f in TARGETS; do
    bf=$(basename $f)
    [ -L ~/.$bf ] && rm -f ~/.$bf || true
    ln -s $f ~/.$bf
    ;;
done

popd
