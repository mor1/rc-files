#!/usr/bin/env bash

INSTALL_DIR=$(pwd)
for f in "${INSTALL_DIR}/*"; do
  [ "$(basename ${f})" = "install.sh" ] && continue
  [ "$(basename ${f})" = "push-env.sh" ] && continue
  [ "$(basename ${f})" = "solarized-dark-mort.itermcolors" ] && continue
  [ -L "~/.$(basename ${f})" ] && rm -f ~/.$(basename ${f})
  ln -s $f ~/.$(basename ${f})
done
#ln -s ${INSTALL_DIR}/bash_profile ~/.bashrc
#ln -s ${INSTALL_DIR}/bash_profile ~/.bash_profile
