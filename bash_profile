PATH=/bin:/sbin/:/usr/bin:/usr/sbin

[ -r ~/.environment  ] && source ~/.environment
[ -r ~/.bash_aliases ] && source ~/.bash_aliases

if [ -f "$HOME/.ssh/sssha" -a "$PLATFORM" = "Darwin" ]; then
    source $HOME/.ssh/sssha                     \
           -k ~/.ssh/nottingham-key             \
           -k ~/.ssh/packetnet-key              \
           -k ~/.ssh/bitbucket-key              \
           -k ~/.ssh/github-key                 \
           -k ~/.ssh/cambridge-key              \
           -k ~/.ssh/cucl-key
fi

if tty -s; then
    stty sane
    uname -a
    date
    complete -r make # hack until fix bash_completion properly
    echo
fi
