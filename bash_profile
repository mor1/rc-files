shopt -s checkwinsize

PATH=/bin:/sbin/:/usr/bin:/usr/sbin

LHOST=`uname -n`
SHOST=${LHOST%%.*}
KERNEL=`uname -r`
export LHOST SHOST KERNEL

[ -r ~/.environment ]  && source ~/.environment
[ -r ~/.bash_aliases ] && source ~/.bash_aliases

if [ -f "$HOME/.ssh/sssha" ]; then
  source $HOME/.ssh/sssha \
                -k ~/.ssh/nottingham-key \
                -k ~/.ssh/cambridge-key  \
                -k ~/.ssh/hw2-dreamplug-key
fi

if tty -s; then
  stty sane
  uname -a
  date
  cd
  echo
fi

## PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:${PATH}"
## export PATH

## GIT bash completion
source /usr/local/Cellar/git/1.7.9.5/etc/bash_completion.d/git-completion.bash
