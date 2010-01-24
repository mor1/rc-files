#
# Some file related bash shell functions
#
# Copyright (C) 2000 Richard Mortier <mort@cantab.net>.  All Rights
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

#
# recursive grep; note need for quotes, and the existence of GNU grep
#

rgrep () {
  if [ $# -lt 2 ]; then
    echo "Usage: rgrep <pattern> '<filespec1>' '<filespec2>' ..."
    return 1
  fi
  rgrep_pattern=$1 ; shift ; rgrep_fnames= ;
  for n in `find . -type d -print` ; do 
    for i in "$@" ; do 
      rgrep_fnames="$rgrep_fnames"\ $n/"$i" ;
    done ;
    grep -s "$rgrep_pattern" $rgrep_fnames ;
    rgrep_fnames= ;
  done ;
  rgrep_pattern= ; rgrep_fnames= ;
}

#
# specific file grep
#

trawl () { 
  find . \(                                                     \
    -name '*.[chsSyli]' -o -name 'Make*'   -o -name '*.cc'  -o  \
    -name '*.hh'        -o -name '*.tcl'   -o -name '*.idl' -o  \
    -name '*.el'        -o -name '*.mk'    -o -name '*.py'  -o  \
    -name '*.html'      -o -name '*.cpp'   -o -name '*.tex' -o  \
    -name '*.bib'       -o -name 'README*' -o -name '*.ns'  -o  \
    -name '*.cs'        -o -name '*.java'  -o -name '*.php' -o  \
    -name '*.fs'        -o -name '*.fsx'   -o -name '*.fsi' -o  \
    -name '*.cfg'	-o -name '*.xml'   -o -name '*.inc' -o  \
    -name '*.mxml'  -o -name '*.as'                             \
  \) -print0 |                                                  \
  xargs -0 grep -EHns "$@"
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
    xterm -fs 12 -title "RFC ${1}" -e less ~/docs/rfcs/rfc${1}.txt
  else
    case $1 in
      -print|-p) a2ps --highlight-level=normal --columns=4 \
                      --row=1 -l82 --interpret=no \
                      ~/docs/rfcs/rfc${2}.txt ;;
    esac
  fi
}

#
# multi-file nm with grep for $1
#

mnm () {
  for i in *.o ; do echo $i ; nm $i | grep ${1} ; done ;
}

#
# "real" word count; a bit heuristic :-)
#

rwc () { 
  owc=`cat "$1" | wc -l` ;
  cwc=`cat "$1" | egrep -v '(^[ [:cntrl:]]*$|[ [:cntrl:]]*//.*|^[ [:cntrl:]]*/\*|^[ [:cntrl:]]*(\{|\})[ [:cntrl:]]*|\*\*|^[ [:cntrl:]]*\*[ [:cntrl:]]*|^[ [:cntrl:]]*\*.*\*$|^#[ [:cntrl:]]*include)' | wc -l`
  ctwc=`cat "$1" | egrep -v '(CAM_TRACE\(|assert\(|TRC\(|DB\(|ENTER\(|LEAVE\(|^[ [:cntrl:]]*$|[ [:cntrl:]]*//.*|^[ [:cntrl:]]*/\*|^[ [:cntrl:]]*(\{|\})[ [:cntrl:]]*|\*\*|^[ [:cntrl:]]*\*[ [:cntrl:]]*|^[ [:cntrl:]]*\*.*\*$|^#[ [:cntrl:]]*include)' |wc -l`

  echo "Total wc:             ${owc}"
  echo "Real wc:              ${cwc}"
  echo "Real wc less tracing: ${ctwc}"

  echo "${owc} ${cwc} ${ctwc}" | awk -- 'END { print "Propn. real code: " $2/$1*100.0 ; print "Propn. real untraced code: " $3/$2*100.0 }' ;
}
