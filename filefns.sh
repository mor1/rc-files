#
# Some file related bash shell functions
#
# Copyright (C) 2000-2014 Richard Mortier <mort@cantab.net>. All Rights
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
# open emacs appropriately
#

e () {
    SERVER_SOCK=/tmp/emacs-$USER/server
    [ -S $SERVER_SOCK ] && \
        emacsclient -n -s $SERVER_SOCK "$@" || \
            { /Applications/Emacs.app/Contents/MacOS/Emacs "$@" & }
}

emacspkg_update () {
    git rm -r "$1" && \
    git add "$2" && \
    git commit -m "emacs: update $1 to $2"
}


#
# specific file grep
#

trawl () {
  find . \( \
    -name "*.[chsSyli]"\
    -or -name "Make*"\
    -or -name "README*"\
    -or -name "*.bib"\
    -or -name "*.cc" -or -name "*.hh"\
    -or -name "*.xml" -or -name "*.html" -or -name "*.css"\
    -or -name "*.el"\
    -or -name "*.fs" -or -name "*.fs[xi]"\
    -or -name "*.inc"\
    -or -name "*.less"\
    -or -name "*.md"\
    -or -name "*.nix"\
    -or -name "*.php"\
    -or -name "*.py"\
    -or -name "*.tex"\
    -or \( \( -name "*.ml" -or -name "*.ml[yil]" \) -not -path "*/_build/*" \) \
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
    less ~/docs/rfcs/rfc${1}.txt
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

PANDOC_MD="pandoc -S --latex-engine=xelatex \
        -Vgeometry=margin=2cm -Vfontsize=11 -Vmainfont=Constantia"

md2tex () {
    $PANDOC_MD -o ${1%.md}.latex ${1}
}

md2docx () {
    $PANDOC_MD -o ${1%.md}.docx ${1}
}

md2pdf () {
    $PANDOC_MD -o ${1%.md}.pdf ${1}
}

PANDOC_LETTER="pandoc -S --latex-engine=xelatex \
        -Vpapersize=a4paper -Vfontsize=11 -Vmainfont=Constantia \
        -Vdocumentclass=letter -H ~/.pandoc/letter-header.latex \
        -Vgeometry=left=1in,right=1in,top=0.75in,bottom=0.75in"

letter2tex () {
    $PANDOC_LETTER -o ${1%.md}.latex ${1}
}

letter2doc () {
    $PANDOC_LETTER -o ${1%.md}.docx ${1}
}

letter2pdf () {
    $PANDOC_LETTER -o ${1%.md}.pdf ${1}
}


#transpose
#: mort@greyjay:openflow$; for i in $(seq 1 $(awk -v FS="," ' { nf=((NF>nf)?NF:nf) } END {print nf}' controller.dat)) ; do cut -f $i -d "," controller.dat | paste -s - ; done | sed -E '/^[[:space:]]+$/d' > control
