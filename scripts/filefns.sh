#
# Some file related bash shell functions
#
# Copyright (C) 2000-2016 Richard Mortier <mort@cantab.net>. All Rights
# Reserved.
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
# specific file grep
#
trawl () {
  find . \( \(                                                      \
       -name '*.[chsSyli]'                                          \
       -or \( \( -name '*.[tj]s' -or -name '*.jsx' \)               \
              -not -path './dist/*' \)                              \
       -or -name '.env*'                                            \
       -or -name '*.bib'                                            \
       -or -name '*.cc'                                             \
       -or -name '*.css'                                            \
       -or -name '*.el'                                             \
       -or -name '*.ejs'                                            \
       -or -name '*.fs' -or -name '*.fs[xi]'                        \
       -or -name '*.hh'                                             \
       -or -name '*.html'                                           \
       -or -name '*.inc'                                            \
       -or -name '*.json'                                           \
       -or -name '*.less'                                           \
       -or -name '*.md'                                             \
       -or -name '*.nix'                                            \
       -or -name '*.php'                                            \
       -or -name '*.py'                                             \
       -or -name '*.rs'                                             \
       -or -name '*.sh'                                             \
       -or -name '*.tex'                                            \
       -or -name '*.tf'                                             \
       -or -name '*.xml'                                            \
       -or -name '*.yml' -or -name '*.yaml'                         \
       -or -name 'APKBUILD'                                         \
       -or -name 'Dockerfile*'                                      \
       -or -name 'Make*'                                            \
       -or -name 'README*'                                          \
       -or \( -name '*.go' -not -path './vendor/*' \)               \
       -or \( \( -name '*.ml' -or -name '*.ml[yil]' -or -name 'dune*' \) \
         -not -name 'setup.ml' -not -name 'myocamlbuild.ml'         \
       \) \)                                                        \
       -not -path '*/_build/*'                                      \
       -not -path '*/.git/*'                                        \
       -not -path '*/_site/*'                                       \
       -not -path '*/x/*'                                           \
       -not -path '*/target/*'                                      \
       -not -path '*/__pypackages__/*'                              \
       -not -path '*/node_modules/*' \)                             \
       -not -name 'package-lock.json'                               \
       -print0 | xargs -0 grep -EHns "$@"
}

#
# test existence of dangling symlinks
#

symt () {
  local argsdone=0
  local verbose=0
  local quiet=0

  while [ $argsdone != 1 ]; do
    case $1 in
      -q) quiet=1 ;;
      -v) verbose=1 ;;
      *)  argsdone=1 ;;
    esac

    args="$args $1"
    if [ $argsdone = 0 ]; then
      shift
    fi
  done

  for f in "$@"; do
    if [ -L "$f" ] ; then
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

rfc () {
  if [ $# = 1 ]; then
    if [ "$1" = "-index" ]; then
      curl -o ~/docs/rfcs/rfc${1}.txt \
           https://www.rfc-editor.org/rfc/rfc${1}.txt
    elif [ ! -s ~/docs/rfcs/rfc${1}.txt ] ; then
      curl -o ~/docs/rfcs/rfc${1}.txt \
           https://www.rfc-editor.org/rfc/rfc${1}.txt
    fi
    less -i ~/docs/rfcs/rfc${1}.txt
  else
    case $1 in
      -print|-p) a2ps --highlight-level=normal --columns=4 \
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

md2tex () {
  $PANDOC_MD -o ${1%.md}.tex $@
}

md2docx () {
  $PANDOC_MD -o ${1%.md}.docx $@
}

md2pdf () {
  $PANDOC_MD -o ${1%.md}.pdf $@
}

PANDOC_LETTER="\
    $PANDOC_BASE -Vdocumentclass=letter -H ~/.pandoc/letter-header.latex \
      -Vgeometry=left=1in,right=1in,top=0.75in,bottom=0.75in \
    "

letter2tex () {
  $PANDOC_LETTER -o ${1%.md}.tex $@
}

letter2doc () {
  $PANDOC_LETTER -o ${1%.md}.docx $@
}

letter2pdf () {
  $PANDOC_LETTER -o ${1%.md}.pdf $@
}

2up () {
  O2="${1%pdf}2up.pdf"
  OO="${O2%pdf}opt.pdf"
  pdfxup -x 2 -y 1 -m 1cm -o "$O2" "$1"
  pdfcpu optimize "$O2" "$OO"
}


#transpose
#: mort@greyjay:openflow$; for i in $(seq 1 $(awk -v FS="," ' { nf=((NF>nf)?NF:nf) } END {print nf}' controller.dat)) ; do cut -f $i -d "," controller.dat | paste -s - ; done | sed -E '/^[[:space:]]+$/d' > control

#
# open emacs appropriately
#

function ee {
  SERVER_SOCK=/tmp/emacs-$USER/server
  [ -S $SERVER_SOCK ] && emacsclient -n -r -s $SERVER_SOCK "$@"
}

function emacspkg-update {
  ( cd ~/.emacs.d/elpa &&
      git rm "$1-*" &&
      git add "$1-*" &&
      git commit -m "emacs: update \`$1\`"
  )
}

function emacspkg-add {
  ( cd ~/.emacs.d/elpa &&
      git add "$1-*" &&
      git commit -m "emacs: add \`$1\`"
  )
}

function emacspkg-rm {
  ( cd ~/.emacs.d/elpa &&
      git rm "$1-*" &&
      git commit -m "emacs: remove \`$1\`"
  )
}

function emacspkgs-commit-all {
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
# abspath, of sorts handling relative symlinks
#

abspath () {
  p=$1
  pushd . > /dev/null
  if [ -d "$1" ]; then
    cd "$1"
    dirs -l +0
  else
    cd $(dirname $(dirname $(which $p))/$(readlink $p))
    cur_dir=`dirs -l +0`
    if [ "$cur_dir" == "/" ]; then
      echo "$cur_dir`basename \"$1\"`"
    else
      echo "$cur_dir/`basename \"$1\"`"
    fi
  fi
  popd > /dev/null
}

#
# some other randoms that seem too big to be aliases
#

function update-all {
  brew update && brew upgrade --greedy
  opam update -y -u
  rm -f ~/.profile
}

function opam-switch {
  opam switch "$@"
  use-ocaml
}

function use-ocaml {
  unset CAML_LD_LIBRARY_PATH
  if [ \! -z $(which opam) ]; then
    eval $(opam config env)
  fi
}

#
# clone remote
#

function git-cloner {
  git clone "$1"
  B=$(basename -s .git "$1")
  cd $B
  git remote add upstream "$2/$B.git"
  git pull upstream master
}

#
# make dd display progress; deprecated for `status=progress`
#

function ddstatus {
  PID=$(ps -ax | grep "[0-9] dd" | tr -s ' ' | cut -f 1 -d ' ' | xargs)
  sudo kill -INFO $PID
}

#
# network configuration
#

function v6-off {
  while read -r line ; do
    echo "XXX: $line"
  done <<< $(networksetup -listallnetworkservices | tail -n+2)
}

function v6-on {
  for i in "$(networksetup -listallnetworkservices | tail -n+2)"; do
    echo "sudo networksetup -setv6automatic \"$i\" || true"
    sudo networksetup -setv6automatic "$i" || true
  done
}

#
# OSX Logs
#

function logs {
  _predicate=""
  if [ -z "$1" ]; then
    _predicate=--predicate 'senderImagePath contains[cd] "'$1'"'
  fi
  shift

  log stream --style syslog --info $_predicate
}

function logv {
  _predicate=""
  if [ -z "$1" ]; then
    _predicate=--predicate 'senderImagePath contains[cd] "$1"'
  fi
  shift

  log show --style syslog --info $_predicate
}

#
# API invocations
#

ghapi () {
  PS=$(echo "https://api.github.com/$@" | tr -s "/")
  curl -H "Authorization: token $(cat ~/.github/token)" $PS
}

#
# VBox
#

vbox-killall () {
  for m in $(VBoxManage list vms | cut -f 1 -d " " | tr -d '"') ; do
    VBoxManage controlvm $m poweroff ; VBoxManage unregistervm --delete $m ;
  done
}

#
# Kvasira!
#

function kvasira {
  api="https://demo.kvasira.com/api/0.0.1"
  content="Content-Type: application/json"
  auth="Authorization: Bearer"
  token=$(curl -s -H "$content" -H "$auth a07171c0-a95e-11e9-bb4e-89313d1ed85a" "$api/corpora" | \
            jq -r --arg ID "$1" '.data|map(select(.id==$ID))|.[0].token')
  curl -X POST -s -d "{\"doc\": \"$2\"}" -H "$content" -H "$auth $token" "$api/query?query_type=url&k=${3:-5}" \
    | jq -r '.response.results|map("\(.title) - \(.uri)", "\(.summary)", "")|.[]'|sed '$ d';
}
