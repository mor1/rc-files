#
# remote hosts
#

SSH="ssh -K"

SSHFSOPTS="\
  -o follow_symlinks\
  -o auto_cache\
  -o idmap=user\
  -o reconnect\
  -o no_readahead"
SSHFS="sshfs $SSHFSOPTS"

# CUCL

kinit () {
  kid=rmm1002@DC.CL.CAM.AC.UK
  cucl_local=false
  while read IP; do
    if [[ $IP =~ ^128.232. ]]; then
      cucl_local=true
    fi
  done < <(ip -j -4 a | jq -r '.[].addr_info | .[].local')

  if [[ "$cucl_local" != "true" ]]; then
    sudo swanctl --initiate --child CUCL
  fi

  if which cl-krenew; then
    cl-krenew --ensuretgt --fresh --maxout
  else
    $(which kinit) -R $kid || $(which kinit) -f $kid
  fi

  if [[ "$cucl_local" != "true" ]]; then
    sudo swanctl --terminate --ike CUCL --force
  fi
}

binky () {
  $SSH binky.cl
}

tfc () {
  $SSH tfc-app9.cl
}

cronserv () {
  $SSH cron-serv$1.cl
}

cucl () {
  $SSH slogin.cl
}

cuclfs () {
  $SSHFS slogin.cl:/home/rmm1002 ~/l/rmm1002
  $SSHFS slogin.cl:/ ~/l/cucl
}

ely () {
  $SSH ely.cl
}

quoth () {
  $SSH quoth.cl
}

uksystems () {
  $SSH rmm1002@hotcrp.uksystems.org # svr-rmm1002-uksystems-hotcrp.cl
}

office () {
  $SSH daugleddau.cl
}

# home

jackdaw () {
  $SSH -k jackdaw.lan
}
mediapc () {
  $SSH -k root@mediapc.lan
}
mediapcfs() {
  sshfs root@mediapc.lan:/$1 ~/l/mediapc
}

srcf () {
  $SSH -k rmm1002@shell.srcf.net
}
srcffs () {
  sshfs rmm1002@shell.srcf.net:/$1 ~/l/srcf
}
