#
# remote hosts
#

SSH="ssh -K"
CUCL="ely.cl"

# SSHFSOPTS="\
#   -o follow_symlinks\
#   -o auto_cache\
#   -o reconnect\
#   -o defer_permissions\
#   -o noappledouble\
#   -o nolocalcaches\
#   -o no_readahead"
# SSHFS="sshfs $SSHFSOPTS"
# # -o uid=503 -o gid=20"

# CUCL

# aod () {
#   $KINIT
#   $SSH armyofdockerness.cl
# }
# aodfs () {
#   $KINIT
#   $SSHFS armyofdockerness.cl:/ ~/l/aod
# }

_kinit () {
  _A=rmm1002@DC.CL.CAM.AC.UK
  ssh -t $1 "cl-krenew --ensuretgt || kinit $_A || /usr/kerberos/bin/kinit $_A"
}

binky () {
  _kinit binky.cl
  $SSH binky.cl
}

cf () {
  $SSH $CUCL finger $1@hermes.cam.ac.uk
}

cronserv () {
  _kinit cronserv$1.cl
  $SSH cron-serv$1.cl
}

cucl () {
  _kinit $CUCL
  $SSH $CUCL
}
# cuclfs () {
#   _kinit $CUCL
#   $SSHFS $CUCL:/home/rmm1002 ~/l/rmm1002
#   $SSHFS $CUCL:/ ~/l/cucl
# }

ely () {
  _kinit ely.cl
  $SSH ely.cl
}

gitlab () {
  $SSH svr-rmm1002-git.cl
}

uksystems () {
  _kinit svr-rmm1002-uksystems2018.cl
  $SSH svr-rmm1002-uksystems2018.cl
}

# netos () {
#   $KINIT
#   $SSHFS $CUCL:/usr/groups/netos ~/l/netos
# }

office () {
  $KINIT
  $SSH daugleddau.cl
}

lab () {
  H=$1
  _kinit $H.cl
  $SSH $H.cl
}

mcs () {
  _kinit linux.cl.ds.cam.ac.uk
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
