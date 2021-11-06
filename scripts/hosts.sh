#
# remote hosts
#

SSH="ssh -K"
CUCL="ely.cl"

SSHFSOPTS="\
  -o follow_symlinks\
  -o auto_cache\
  -o reconnect\
  -o defer_permissions\
  -o noappledouble\
  -o nolocalcaches\
  -o no_readahead"
# -o uid=503 -o gid=20"
SSHFS="sshfs $SSHFSOPTS"

# CUCL

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

cuclfs () {
  _kinit $CUCL
  $SSHFS $CUCL:/home/rmm1002 ~/l/rmm1002
  $SSHFS $CUCL:/ ~/l/cucl
}

ely () {
  _kinit ely.cl
  $SSH ely.cl
}

gitlab () {
  _kinit $CUCL
  $SSH svr-rmm1002-git.cl
}

quoth () {
  _kinit quoth.cl
  $SSH quoth.cl
}

uksystems () {
  _kinit svr-rmm1002-uksystems-hotcrp.cl
  $SSH rmm1002@hotcrp.uksystems.org # svr-rmm1002-uksystems-hotcrp.cl
}

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
