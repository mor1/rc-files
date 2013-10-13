alias ls="/bin/ls -FC"
alias ll="ls -l"
alias lll="ls -lT"
alias la="ls -A"
alias lla="ls -lA"
alias l="ls"

alias g="grep -Hn"
alias less="less -FRXi"
alias m="less -E"

alias cu="ci -u"
alias bc="bc -l"

alias src2ps="a2ps --highlight-level=normal --columns=4 --rows=1 -L190 -l90"
alias rfc2ps="a2ps --highlight-level=normal --columns=4 --row=1 -l82 --interpret=no"

alias ctime="python -c 'import sys,time; print time.ctime(float(sys.argv[1]))'"
alias bcat="hexdump -C"

alias github-tunnel="ssh -f -N github-tunnel"
alias irc-tunnel="ps aux | \grep 'ssh -f -N irc-tunnel' | grep -v grep | tr -s ' ' | cut -d ' ' -f 2 | xargs kill -9 ; ssh -f -N irc-tunnel"

alias curl="curl --noproxy localhost,127.0.0.1"

alias coffee="rlwrap coffee"

alias use-rvm='aenv PATH ~/.rvm/bin && aenv PATH ~/.rvm/gems/ruby-1.9.3-p125/bin && [ -s "$HOME/.rvm/scripts/rvm" ] && source "$HOME/.rvm/scripts/rvm" && rvm use 1.9.3@global'
alias use-ocaml='unset CAML_LD_LIBRARY_PATH && which opam && eval $(opam config env)'
alias use-proxy='export http_proxy=http://wwwcache.cs.nott.ac.uk:3128'
alias no-proxy='unset http_proxy'

alias pdfmerge="/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py"
alias qv="qlmanage -p"
alias emacs="open /usr/local/Cellar/emacs/24.2/Emacs.app"
alias nixos="source /Users/$(whoami)/.nix-profile/etc/profile.d/nix.sh && export NIX_PATH=/nix/var/nix/profiles/per-user/$(whoami)/channels/nixos"
