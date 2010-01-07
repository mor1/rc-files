##
## $Id: .bash_aliases,v 1.3 2009/06/22 14:42:46 mort Exp $
##

alias ls="/bin/ls -FC"
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -lA"
alias l="ls"
alias more="less -X"
alias cu="ci -u"
alias bc="bc -l"
alias gm="gmake"
alias m="more"
alias src2ps="a2ps --highlight-level=normal --columns=4 --rows=1 -L190 -l90" 
alias rfc2ps="a2ps  --highlight-level=normal --columns=4 --row=1 -l82 --interpret=no" 
alias ctime="python -c 'import sys,time; print time.ctime(float(sys.argv[1]))'"

