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
alias irc-tunnel="ps aux | grep irc-tunnel | grep -v 'grep irc-tunnel' | tr -s ' ' | cut -d ' ' -f 2 | xargs kill -9 ; ssh -f -N irc-tunnel"

alias curl="curl --noproxy localhost,127.0.0.1"

alias ocaml="rlwrap ocaml"
alias use-rvm='aenv PATH ~/.rvm/bin && aenv PATH ~/.rvm/gems/ruby-1.9.3-p125/bin && [ -s "$HOME/.rvm/scripts/rvm" ] && source "$HOME/.rvm/scripts/rvm"'

alias pdfmerge="/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py"
