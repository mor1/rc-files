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
    find . \(                                                       \
         -name '*.[chsSyli]'                                        \
         -or -name 'Make*'                                          \
         -or -name 'Dockerfile'                                     \
         -or -name 'README*'                                        \
         -or -name '*.bib'                                          \
         -or -name '*.cc' -or -name '*.hh'                          \
         -or -name '*.xml' -or -name '*.html' -or -name '*.css'     \
         -or -name '*.el'                                           \
         -or -name '*.fs' -or -name '*.fs[xi]'                      \
         -or -name '*.go'                                           \
         -or -name '*.inc'                                          \
         -or -name '*.less'                                         \
         -or -name '*.md'                                           \
         -or -name '*.nix'                                          \
         -or -name '*.php'                                          \
         -or -name '*.py'                                           \
         -or -name '*.tex'                                          \
         -or \( \( -name '*.ml' -or -name '*.ml[yil]' \)            \
               -not -path '*/_build/*'                              \
               -not -name 'setup.ml' -not -name 'myocamlbuild.ml'   \
               \)                                                   \
         \) -print0 | xargs -0 grep -EHns "$@"
}

#
# test existence of dangling symlinks; returns 1 on ok, 0 on dangling
#

symt () {

    local retval=0
    local argsdone=0
    local verbose=0
    local cmd="echo"
    local subdirs=0
    local args=""

    if [ $# = 0 ]; then
        echo 'symt: test for dangling symlinks' ;
        echo 'Usage: symt [option] <filename>' ;
        echo '       -q  : quiet (just set return value)' ;
        echo '       -d  : delete file *** CAUTION ***' ;
        echo '       -r  : recurse down subdirectories' ;
        echo '       -v  : report ok symlinks as well' ;
        echo '       -vv : as -v, report non-symlinks as well' ;
    fi

    while [ $argsdone != 1 ]; do
        case $1 in
            -q)  cmd=":"     ; args="$args $1" ; shift ;;
            -vv) verbose=2   ; args="$args $1" ; shift ;;
            -v)  verbose=1   ; args="$args $1" ; shift ;;
            -r)  subdirs=1   ; args="$args $1" ; shift ;;
            -d)  cmd="rm -f" ; args="$args $1" ; shift ;;
            *)  argsdone=1 ;;
        esac ;
    done

    while [ -n "$1" ]; do
        [ -L $1 ]                          \
            && { [ ! -r $1 ]                   \
                     && { eval "$cmd $1 : dangling" ;    \
                          retval=0 ;
                 }             \
                     || { [ $verbose = 1 ]         \
                              && { eval "$cmd $1 : ok" ; } ; \
                          retval=1 ; } ; }         \
            || { [ $verbose = 2 ]              \
                     && { eval "$cmd $1 not a symlink" ; } ; } ;

        [ $subdirs = 1 ] && [ -d $1 ] && [ "$1" != ".." ] && [ "$1" != "." ] \
            && { symt $args ${1}/{*,.*} ; };

        shift ;
    done

    return $retval
}

#
# read/print an RFC
#

rfc () {
    if [ $# = 1 ]; then
        if [ "$1" = "-index" ]; then
            curl -o ~/docs/rfcs/rfc${1}.txt http://www.rfc-editor.org/rfc/rfc${1}.txt
        elif [ ! -s ~/docs/rfcs/rfc${1}.txt ] ; then
            curl -o ~/docs/rfcs/rfc${1}.txt http://www.rfc-editor.org/rfc/rfc${1}.txt
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

PANDOC_BASE='
  d mor1/pandoc -S --latex-engine=xelatex -Vfontsize=12 -Vpapersize=a4paper'

PANDOC_MD="$PANDOC_BASE --number-sections -Vgeometry=margin=2cm"

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


#transpose
#: mort@greyjay:openflow$; for i in $(seq 1 $(awk -v FS="," ' { nf=((NF>nf)?NF:nf) } END {print nf}' controller.dat)) ; do cut -f $i -d "," controller.dat | paste -s - ; done | sed -E '/^[[:space:]]+$/d' > control

#
# open emacs appropriately
#

e () {
    SERVER_SOCK=/tmp/emacs-$USER/server
    [ -S $SERVER_SOCK ] && \
        emacsclient -n -s $SERVER_SOCK "$@" || \
            { /Applications/Emacs.app/Contents/MacOS/Emacs "$@" & }
}

function emacspkg-update {
    ( cd ~/rc-files/emacs.d/elpa &&
            git rm "$1-*" &&
            git add "$1-*" &&
            git commit -m "emacs: update \`$1\`"
    )
}

function emacspkg-add {
    ( cd ~/rc-files/emacs.d/elpa &&
            git add "$1-*" &&
            git commit -m "emacs: add \`$1\`"
    )
}

function emacspkg-rm {
    ( cd ~/rc-files/emacs.d/elpa &&
            git rm "$1-*" &&
            git commit -m "emacs: remove \`$1\`"
    )
}

function emacspkgs-commit-all {
    for n in $(git st | grep -E "^\s+emacs.d.*/$" | cut -f 3 -d "/") ; do
        emacspkg-update ${n%-*}
    done
    for n in $(git st | grep -E "^\s+emacs.d.*/$" | cut -f 3 -d "/") ; do
        emacspkg-add ${n%-*}
    done
    ( cd ~/rc-files/emacs.d/elpa/archives \
            && git commit -m "emacs: update archives" */archive-contents*
    )
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
    brew update \
        && brew upgrade && brew cleanup \
        && brew upgrade brew-cask && brew cask cleanup
    opam update -y -u
    rm -f ~/.profile
}

function use-rvm {
    aenv PATH ~/.rvm/bin
    aenv PATH ~/.rvm/gems/ruby-2.1.0/bin
    aenv PATH ~/.rvm/gems/ruby-2.1.0@global/bin
    if [ -s "$HOME/.rvm/scripts/rvm" ]; then
        source "$HOME/.rvm/scripts/rvm"
        rvm use 2.1.0@global
    fi
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
    git remote add upstream "git://github.com/$2/$B.git"
    git pull upstream master
}

#
# make dd display progress
#

function ddstatus {
    PID=$(ps -ax | grep "[0-9] dd" | tr -s ' ' | cut -f 1 -d ' ' | xargs)
    sudo kill -INFO $PID
}

#
# photo manipulation
#

function photos-rename {
    jhead -exonly -ft -n%Y%m%d-%H%M%S-%03i "$@"
}

function photos-subdir {
    for n in *.jpg ; do
        d=${n%%-*}-- && ( [ \! -d $d ] && mkdir $d || true ) && mv $n $d
    done
    # for n in *.jpg ; do
    #    mv $n $(echo $n | cut -b1-4)-$(echo $n | cut -b5-6)-$(echo $n | cut -b7-8)--
    # done
}

function photos-mirror {
    rsync -av --delete ./pictures/ ~/l/mediapc/storage/pictures
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
