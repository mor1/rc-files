#!/usr/bin/env bash
#
# Copyright (c) Richard Mortier <mort@cantab.net>
#
# Permission to use, copy, modify, and distribute this software for any purpose
# with or without fee is hereby granted, provided that the above copyright
# notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

set -e

echo $(date) offlineimap: post-sync

if [ ! -d "~/.cache/mu" ]; then
  mu init --maildir=~/u/me/mail \
     --my-address=mort@cantab.net \
     --my-address=mort@greyjay \
     --my-address=mort@ikva.ai \
     --my-address=mort@kvasir.com \
     --my-address=mort@kvasira.com \
     --my-address=mort@live.co.uk \
     --my-address=mort@microsoft.com \
     --my-address=mort@sprintlabs.com \
     --my-address=mort@vipadia.com \
     --my-address=pszrmm@exmail.nottingham.ac.uk \
     --my-address=pszrmm@nottingham.ac.uk \
     --my-address=richard.mortier@cdbb.cam.ac.uk \
     --my-address=richard.mortier@cl.cam.ac.uk \
     --my-address=richard.mortier@cst.cam.ac.uk \
     --my-address=richard.mortier@docker.com \
     --my-address=richard.mortier@gmail.com \
     --my-address=richard.mortier@hotmail.com \
     --my-address=richard.mortier@ikva.ai \
     --my-address=richard.mortier@nottingham.ac.uk \
     --my-address=richard.mortier@unikernel.com \
     --my-address=rmm1002@cam.ac.uk \
     --my-address=rmm1002@cl.cam.ac.uk \
     --my-address=rmm1002@cst.cam.ac.uk \
     --my-address=rmm1002@hermes.cam.ac.uk \
     --my-address=rmm1002@srcf.net \
     --my-address=rmm@cs.nott.ac.uk
fi

if [ "$1" == "--index" ] ; then
  mu index --quiet
fi

logger -t offlineimap -p mail.info "Sync of mail account completed"
