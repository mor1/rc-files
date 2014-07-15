#!/usr/bin/env bash
#
# Copyright (C) 2000--2014 Richard Mortier <mort@cantab.net>.  All Rights
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

## capture any existing authorized keys
[ -s ~/.ssh/authorized_keys ] && mv ~/.ssh/authorized_keys ssh

INSTALL_DIR=$(pwd)
for f in ${INSTALL_DIR}/*; do
  bf=$(basename $f)
  case "$bf" in
      "floatlg.jpg" | "install.sh" | "push-env.sh" | "envfns.sh"    \
          | "solarized-dark-mort.itermcolors" | "macbook-uk.layout" \
          )

          ;;

      * )
          [ -L ~/.$bf ] && rm -f ~/.$bf || true
          ln -s $f ~/.$bf
          ;;
  esac
done

## i value consistency in my environemnts. so what?
rm -f ~/.bashrc
ln -s ${INSTALL_DIR}/bash_profile ~/.bashrc
