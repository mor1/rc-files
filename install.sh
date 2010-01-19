#!/usr/bin/env bash

INSTALL_DIR=$(pwd)
for f in ${INSTALL_DIR}/*; do
  [ "$(basename ${f})" = "install.sh" ] && continue
  [ "$(basename ${f})" = "push-env.sh" ] && continue
  rm -f ~/.$(basename ${f}) && ln -s $f ~/.$(basename ${f})
done
