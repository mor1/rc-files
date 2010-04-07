alias ls="/bin/ls -FC"
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -lA"
alias l="ls"

alias more="less -X"
alias m="more"

alias cu="ci -u"
alias bc="bc -l"

alias src2ps="a2ps --highlight-level=normal --columns=4 --rows=1 -L190 -l90" 
alias rfc2ps="a2ps --highlight-level=normal --columns=4 --row=1 -l82 --interpret=no" 

alias ctime="python -c 'import sys,time; print time.ctime(float(sys.argv[1]))'"
alias bcat="hexdump -C"

alias github-tunnel="ssh -f -N github-tunnel"
alias nottsmtp-tunnel-stop="ps aux | grep nottsmtp-tunnel | grep -v 'grep nottsmtp-tunnel' | tr -s ' ' | cut -d ' ' -f 2 | sudo xargs kill -9"
alias nottsmtp-tunnel='sudo kill -9 $(ps aux | grep nottsmtp-tunnel | grep -v "grep nottsmtp-tunnel" | tr -s " " | cut -d " " -f 2) ; sudo ssh -f -N nottsmtp-tunnel'

alias curl="curl --noproxy localhost,127.0.0.1"
