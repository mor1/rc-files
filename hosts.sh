#
# remote hosts
#

SSH="ssh -K"

SLOGIN="slogin"
CLOGIN="slogin -K"

CUCL=ely.cl
ACCT=rmm1002@AD.CL.CAM.AC.UK

KINIT="cl-krenew -q --ensuretgt --maxout"                      # common case
# KINIT="$KINIT || kinit $ACCT || /usr/kerberos/bin/kinit $ACCT" # oddities
KINIT="ssh -tx rmm1002@slogin.cl.cam.ac.uk $KINIT" # run on SSH gateway

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
    $CLOGIN armyofdockerness.cl
}
aodfs () {
    $KINIT
    $SSHFS armyofdockerness.cl:/ ~/l/aod
}

cf () {
  $SSH $CUCL finger $1@hermes.cam.ac.uk
}

cron-serv () {
    $KINIT
    $CLOGIN cron-serv$1.cl
}

cucl () {
    $KINIT
    $CLOGIN $CUCL
}
cuclfs () {
    $KINIT
    $SSHFS $CUCL:/home/rmm1002 ~/l/rmm1002
    $SSHFS $CUCL:/ ~/l/cucl
}

gitlab () {
    $KINIT
    $CLOGIN svr-rmm1002-git.cl
}

netos () {
    $KINIT
    $SSHFS $CUCL:/usr/groups/netos ~/l/netos
}

office () {
    $KINIT
    $CLOGIN daugleddau.cl
}

lab () {
    H=$1
    $KINIT
    $CLOGIN $H.cl
}

# Nottingham

homeiot () {
    $SLOGIN berezin.mrl.nottingham.ac.uk
}
homeiotfs () {
    sshfs berezin.mrl.nottingham.ac.uk:/$1 ~/l/homeiot
}

marian () {
    $SLOGIN marian.cs.nott.ac.uk
}
marianfs () {
    sshfs marian.cs.nott.ac.uk:/$1 ~/l/marian
}

paws () {
    slogin paws-server
}
pawsfs () {
    sshfs paws-server:/$1 ~/l/paws
}

severn () {
    $SLOGIN severn.cs.nott.ac.uk
}
severnfs () {
    sshfs severn.cs.nott.ac.uk:/$1 ~/l/severn
}

stratus () {
    $SLOGIN stratus.horizon.ac.uk
}
stratusfs () {
    sshfs -o uid=503 -o gid=20 stratus.cs.nott.ac.uk:/$1 ~/l/stratus
}

ucn () {
    $SLOGIN ucn-server
}

# Other

mediapc () {
    $SLOGIN root@mediapc.home
}
mediapcfs() {
    sshfs root@mediapc.home:/$1 ~/l/mediapc
}

mobydev () {
    $SLOGIN moby-dev.packet
}
mobydevfs () {
    $SSHFS moby-dev.packet:/$1 ~/l/mobydev
}

monk () {
    $SLOGIN monk.recoil.org
}

punk () {
    $SLOGIN punk.recoil.org
}

srcf () {
    $SLOGIN rmm1002@shell.srcf.net
}
srcffs () {
    sshfs rmm1002@shell.srcf.net:/$1 ~/l/srcf
}

stmwww () {
    slogin -p 722 stthnorg@stthomasmorewollaton.org.uk
}
stmwwwfs() {
    sshfs -p 722 stthnorg@stthomasmorewollaton.org.uk: ~/l/stm-www
}

vagrantfs() {
    sshfs -p 2222 vagrant@localhost: ~/l/vagrant
}

xen () {
    $SLOGIN -p 2233 localhost
}
xenfs () {
    sshfs -p 2233 localhost: ~/l/xen
}
