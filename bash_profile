PATH=/bin:/sbin/:/usr/bin:/usr/sbin

[ -r ~/.environment  ] && source ~/.environment
[ -r ~/.bash_aliases ] && source ~/.bash_aliases

if [ -f "$HOME/.ssh/sssha" ]; then
    source $HOME/.ssh/sssha \
           -k ~/.ssh/nottingham-key \
           -k ~/.ssh/bitbucket-key \
           -k ~/.ssh/stmwww-key \
           -k ~/.ssh/cambridge-key
fi

if tty -s; then
    stty sane
    uname -a
    date
    cd
    echo
fi
