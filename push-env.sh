#!/usr/bin/env bash

DEST=$1
UNAME=$2
if [ ! -z "${UNAME}" ]; then UNAME=${UNAME}@ ; fi

ssh ${UNAME}${DEST} "mkdir -p ~/src"
ssh ${UNAME}${DEST} "rm -rf ~/src/sh-scripts.git"
scp -r ~/src/sh-scripts.git ${UNAME}${DEST}:~/src/sh-scripts.git

ssh ${UNAME}${DEST} "mkdir -p ~/install"
ssh ${UNAME}${DEST} "rm -rf ~/install/config.git"
scp -r ~/install/config.git ${UNAME}${DEST}:~/install/config.git
            
# ssh ${DEST} mkdir -p /home/mort/src
# ssh ${DEST} "chmod -R u+w ~/src/sh/*"
# ssh ${DEST} "chmod u+w ~/.bash_aliases ~/.bash_profile ~/.bashrc ~/.environment"

# scp -r ${HOME}/src/sh ${DEST}:~/src/
# scp -r ${HOME}/.ssh/sssha ${DEST}:~/.ssh/

# scp ${HOME}/.bash_aliases ${HOME}/.bash_profile ${HOME}/.bashrc ${HOME}/.environment ${DEST}:~
