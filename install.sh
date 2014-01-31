#!/usr/bin/env bash
set -ex

INSTALL_DIR=$(pwd)
for f in ${INSTALL_DIR}/*; do
  bf=$(basename $f)
  case "$bf" in
  "floatlg.jpg" | "install.sh" | \
    "push-env.sh" | "solarized-dark-mort.itermcolors" | \
    "emacs.dead" | "macbook-uk.layout" )
    ;;
  * )
    [ -L ~/.$bf ] && rm -f ~/.$bf || true
    ln -s $f ~/.$bf
    ;;
  esac
done
#ln -s ${INSTALL_DIR}/bash_profile ~/.bashrc
#ln -s ${INSTALL_DIR}/bash_profile ~/.bash_profile
