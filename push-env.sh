#!/bin/sh

DEST=$1

ssh ${DEST} mkdir -p /home/mort/src
ssh ${DEST} "chmod -R u+w ~/src/sh/*"
ssh ${DEST} "chmod u+w ~/.bash_aliases ~/.bash_profile ~/.bashrc ~/.environment"

scp -r ${HOME}/src/sh ${DEST}:~/src/
scp -r ${HOME}/.ssh/sssha ${DEST}:~/.ssh/

scp ${HOME}/.bash_aliases ${HOME}/.bash_profile ${HOME}/.bashrc ${HOME}/.environment ${DEST}:~
