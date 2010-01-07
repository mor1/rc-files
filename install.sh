#!/bin/sh
INSTALL_DIR=~/install/config.git

for f in ${INSTALL_DIR}/*; do
  [ "$(basename ${f})" = "install.sh" ] && continue
  ln -s $f ~/.$(basename ${f})
done
