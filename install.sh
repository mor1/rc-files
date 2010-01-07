#!/bin/sh
INSTALL_DIR=$(pwd)
for f in ${INSTALL_DIR}/*; do
  [ "$(basename ${f})" = "install.sh" ] && continue
  rm -f ~/.$(basename ${f}) && ln -s $f ~/.$(basename ${f})
done
