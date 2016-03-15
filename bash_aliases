alias ls="/bin/ls -FC"
alias ll="ls -l"
alias lll="ls -lT"
alias la="ls -A"
alias lla="ls -lA"
alias l="ls"

alias less="less -FRXi"
alias m="less -E"
alias bc="bc -l"

alias src2ps="a2ps --highlight-level=normal --columns=4 --rows=1 -L190 -l90"
alias rfc2ps="a2ps --highlight-level=normal --columns=4 --row=1 -l82 --interpret=no"

alias ctime="python -c 'import sys,time; print time.ctime(float(sys.argv[1]))'"
alias bcat="hexdump -C"

alias github-tunnel="ssh -f -N github-tunnel"
alias irc-tunnels="ps aux | \grep -E -- '-tunnel$' | tr -s ' ' | cut -d ' ' -f 2 | xargs kill -9 ; ssh -f -N irc-tunnel ; ssh -f -N opw-tunnel"

alias qv="qlmanage -px"

alias eject="diskutil unmount $1"
alias reset="\reset -i ^C"
