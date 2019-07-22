#
# remote hosts
#

SSH="ssh -K"
CUCL="slogin.cl"

KINIT="cl-krenew -q --ensuretgt" # common case
ACCT=rmm1002@DC.CL.CAM.AC.UK
KINIT="$KINIT || kinit $ACCT || /usr/kerberos/bin/kinit $ACCT" # oddities
KINIT="ssh -tx $CUCL $KINIT" # run on SSH gateway

SSHFSOPTS="\
  -o follow_symlinks\
  -o auto_cache\
  -o reconnect\
  -o defer_permissions\
  -o noappledouble\
  -o nolocalcaches\
  -o no_readahead"
SSHFS="sshfs $SSHFSOPTS"
# -o uid=503 -o gid=20"

# CUCL

aod () {
  $KINIT
  $SSH armyofdockerness.cl
}
aodfs () {
  $KINIT
  $SSHFS armyofdockerness.cl:/ ~/l/aod
}

binky () {
  $KINIT
  $SSH binky.cl
}

cf () {
  $SSH $CUCL finger $1@hermes.cam.ac.uk
}

cronserv () {
  $KINIT
  $SSH cron-serv$1.cl
}

cucl () {
  $KINIT
  $SSH $CUCL
}
cuclfs () {
  $KINIT
  $SSHFS $CUCL:/home/rmm1002 ~/l/rmm1002
  $SSHFS $CUCL:/ ~/l/cucl
}

gitlab () {
  $KINIT
  $SSH svr-rmm1002-git.cl
}

uksystems () {
  $KINIT
  $SSH svr-rmm1002-uksystems2018.cl
}

netos () {
  $KINIT
  $SSHFS $CUCL:/usr/groups/netos ~/l/netos
}

office () {
  $KINIT
  $SSH daugleddau.cl
}

lab () {
  H=$1
  $KINIT
  $SSH $H.cl
}

mcs () {
  $KINIT
  $SSH linux.cl.ds.cam.ac.uk
}

# Other

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
