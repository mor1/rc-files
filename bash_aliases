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
alias nottsmtp-tunnel='sudo ssh -i /Users/mort/.ssh/nottingham-key -f -N nottsmtp-tunnel'

alias irc-tunnel-stop="ps aux | grep irc-tunnel | grep -v 'grep irc-tunnel' | tr -s ' ' | cut -d ' ' -f 2 | xargs kill -9"
alias irc-tunnel='ssh -f -N irc-tunnel'

alias curl="curl --noproxy localhost,127.0.0.1"
alias update-gits='for n in *.git ; do cd $n ; git pull ; cd .. ; done'

alias odb="ocaml ~/research/mirage/src/odb/odb.ml"
alias ocaml="rlwrap ocaml"
