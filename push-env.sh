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

DEST=$1
USER=$2
if [ ! -z "${UNAME}" ]; then UNAME=${UNAME}@ ; fi

ssh ${USER}${DEST}                              \
    "mkdir -p ~/src                             \
      && cd ~/src                               \
      && git clone ${RCFILES}                   \
      && ./install.sh                           \
      && git clone ${SHFILES} "
