#
# Some file related bash shell functions
#
# Copyright (C) Richard Mortier <mort@cantab.net>. All Rights Reserved.
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License version 2 as published by the Free
# Software Foundation
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 59 Temple
# Place - Suite 330, Boston, MA 02111-1307, USA.

#
# test existence of dangling symlinks
#

function symt() {
  local argsdone=0
  local verbose=0
  local quiet=0

  while [ $argsdone != 1 ]; do
    case $1 in
      -q) quiet=1 ;;
      -v) verbose=1 ;;
      *) argsdone=1 ;;
    esac

    args="$args $1"
    if [ $argsdone = 0 ]; then
      shift
    fi
  done

  for f in "$@"; do
    if [ -L "$f" ]; then
      if [ ! -r $f ]; then
        echo "$f: dangling"
        retval=1
      else
        if [ $quiet = 0 ]; then
          echo "$f: ok"
          retval=0
        fi
      fi
    else
      if [ $verbose = 1 ]; then
        echo "$f: not a symlink"
        retval=0
      fi
    fi
  done
  return $retval
}

#
# read/print an RFC
#

function rfc() {
  if [ $# = 1 ]; then
    if [ "$1" = "-index" ]; then
      curl -o ~/docs/rfcs/rfc${1}.txt \
        https://www.rfc-editor.org/rfc/rfc${1}.txt
    elif [ ! -s ~/docs/rfcs/rfc${1}.txt ]; then
      curl -o ~/docs/rfcs/rfc${1}.txt \
        https://www.rfc-editor.org/rfc/rfc${1}.txt
    fi
    less -i ~/docs/rfcs/rfc${1}.txt
  else
    case $1 in
      -print | -p) a2ps --highlight-level=normal --columns=4 \
        --row=1 -l82 --interpret=no \
        ~/docs/rfcs/rfc${2}.txt ;;
    esac
  fi
}

#
# pandoc invocations
#

PANDOC_BASE='pandoc --from=markdown+smart -Vpapersize=a4 --pdf-engine=lualatex'

PANDOC_MD="$PANDOC_BASE -Vgeometry=margin=2cm -Vcolorlinks"

function md2tex() {
  $PANDOC_MD -o ${1%.md}.tex $@
}

function md2docx() {
  $PANDOC_MD -o ${1%.md}.docx $@
}

function md2pdf() {
  $PANDOC_MD -o ${1%.md}.pdf $@
}

PANDOC_LETTER="\
    $PANDOC_BASE -Vdocumentclass=letter -H ~/.pandoc/letter-header.latex \
      -Vgeometry=left=1in,right=1in,top=0.75in,bottom=0.75in \
    "

function letter2tex() {
  $PANDOC_LETTER -o ${1%.md}.tex $@
}

function letter2doc() {
  $PANDOC_LETTER -o ${1%.md}.docx $@
}

function letter2pdf() {
  $PANDOC_LETTER -o ${1%.md}.pdf $@
}

function 2up() {
  O2="${1%pdf}2up.pdf"
  OO="${O2%pdf}opt.pdf"
  pdfxup -x 2 -y 1 -m 1cm -o "$O2" "$1"
  # pdfcpu optimize "$O2" "$OO"
  # mv "$O2" "$1"
}

#transpose
#: mort@greyjay:openflow$; for i in $(seq 1 $(awk -v FS="," ' { nf=((NF>nf)?NF:nf) } END {print nf}' controller.dat)) ; do cut -f $i -d "," controller.dat | paste -s - ; done | sed -E '/^[[:space:]]+$/d' > control

#
# open emacs appropriately
#

function e() {
  SERVER_SOCK=/tmp/emacs-$USER/server
  [ -S $SERVER_SOCK ] && emacsclient -n -r -s $SERVER_SOCK "$@"
}

function ee() {
  SERVER_SOCK=/tmp/emacs-$USER/server
  [ -S $SERVER_SOCK ] && emacsclient -n -c -s $SERVER_SOCK "$@"
}

function emacspkg-update() {
  (
    cd ~/.emacs.d/elpa \
      && git rm "$1-*" \
      && git add "$1-*" \
      && git commit -m "emacs: update \`$1\`"
  )
}

function emacspkg-add() {
  (
    cd ~/.emacs.d/elpa \
      && git add "$1-*" \
      && git commit -m "emacs: add \`$1\`"
  )
}

function emacspkg-rm() {
  (
    cd ~/.emacs.d/elpa \
      && git rm "$1-*" \
      && git commit -m "emacs: remove \`$1\`"
  )
}

function emacspkgs-commit-all() {
  pushd ~/.emacs.d
  PKGS=$(git status -s | grep -E "^   elpa/.*$" | cut -f 2 -d "/" | uniq)
  for n in $PKGS; do
    emacspkg-update ${n%-*}
  done
  PKGS=$(git status -s | grep -E "^[?][?] elpa/.*/$" | cut -f 2 -d "/" | uniq)
  for n in $PKGS; do
    emacspkg-add ${n%-*}
  done
  PKGS=$(git status -s | grep -E "^ D elpa/.*$" | cut -f 2 -d "/" | uniq)
  for n in $PKGS; do
    emacspkg-rm ${n%-*}
  done
  popd
}

#
# make dd display progress; deprecated for `status=progress`
#

function ddstatus() {
  PID=$(ps -ax | grep "[0-9] dd" | tr -s ' ' | cut -f 1 -d ' ' | xargs)
  sudo kill -INFO $PID
}

#
# reverse find
#

function rf() {
  while ! eza -1 "${D:=.}/$1" 2> /dev/null; do
    D=$D/..
    [ "$(realpath "$D/$1")" == "/$1" ] && break
  done
  unset D
}

#
# resolve url
#

function unsl() {
  curl -v "$1" 2>&1 | rg -i location | cut -d" " -f3
}
