#
# Some Docker related bash shell functions
#
# Copyright (C) 2016 Richard Mortier <mort@cantab.net>. All Rights Reserved.
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

function docker-kill {
    docker ps --all --quiet | xargs docker stop -t 2
    docker ps --all --quiet | xargs docker rm
}

function docker-clean {
    docker images | grep "<none>" | tr -s " " | cut -d" " -f 3 \
        | xargs docker rmi
}

function d {
    docker run -ti --rm -v $(pwd -P):/cwd -w /cwd "$@"
}

function linux {
    d alpine sh -c "$*"
}

function coffee {
    d mor1/alpine-coffeescript:latest "$@"
}

function jekyll {
    d mor1/alpine-jekyll:latest "$@"
}

function python3 {
    d mor1/alpine-python3:latest "$@"
}

function casperjs {
    d casperjs-local casperjs "$@"
}
