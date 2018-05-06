PATH=/bin:/sbin/:/usr/bin:/usr/sbin

[ -r ~/.environment ] && source ~/.environment

if [ -f "$HOME/.ssh/sssha" -a "$PLATFORM" = "Darwin" ]; then
    source $HOME/.ssh/sssha                     \
           -k ~/.ssh/nottingham-key             \
           -k ~/.ssh/nottingham-key-rsa         \
           -k ~/.ssh/packetnet-key              \
           -k ~/.ssh/bitbucket-key              \
           -k ~/.ssh/bitbucket-key-rsa          \
           -k ~/.ssh/github-key                 \
           -k ~/.ssh/cambridge-key              \
           -k ~/.ssh/cucl-key
fi

if tty -s; then
    stty sane
    uname -a
    date
    [ "$PLATFORM" = "Darwin" ] && complete -r make # hack
    echo
fi
