##
## $Id: .bash_profile,v 1.5 2008/06/23 10:41:22 mort Exp mort $
##

shopt -s checkwinsize

PATH=/bin:/sbin/:/usr/bin:/usr/sbin

LHOST=`uname -n`
SHOST=${LHOST%%.*}
KERNEL=`uname -r`
export LHOST SHOST KERNEL

[ -r ~/.environment ]  && source ~/.environment
[ -r ~/.bash_aliases ] && source ~/.bash_aliases

if [ -f "$HOME/.ssh/sssha" ]; then
  source $HOME/.ssh/sssha -k ~/.ssh/vipadia-key -k ~/.ssh/nott-key
fi

if tty -s; then
  stty sane
  uname -a
  date
  cd
  echo
fi

# Setting PATH for MacPython 2.5
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:${PATH}"
export PATH

##
# Your previous /Users/mort/.bash_profile file was backed up as /Users/mort/.bash_profile.macports-saved_2009-07-15_at_17:43:21
##

# MacPorts Installer addition on 2009-07-15_at_17:43:21: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.


# MacPorts Installer addition on 2009-07-15_at_17:43:21: adding an appropriate MANPATH variable for use with MacPorts.
export MANPATH=/opt/local/share/man:$MANPATH
# Finished adapting your MANPATH environment variable for use with MacPorts.

##
# Your previous /Users/mort/.bash_profile file was backed up as /Users/mort/.bash_profile.macports-saved_2010-01-05_at_14:04:20
##

# MacPorts Installer addition on 2010-01-05_at_14:04:20: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

