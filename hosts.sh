#
# remote hosts
#

SSH="ssh -KX"

SLOGIN="slogin"
CLOGIN="slogin -KX"
if [ "${LHOST%.cam.ac.uk}" = "$LHOST" ]; then
    CLOGIN="ssh -At slogin.cl ssh -t"
fi

CUCL=ely.cl
ACCT=rmm1002@AD.CL.CAM.AC.UK

KINIT="cl-krenew -q --ensuretgt --maxout"                      # common case
KINIT="$KINIT || kinit $ACCT || /usr/kerberos/bin/kinit $ACCT" # oddities
KINIT="ssh -tx rmm1002@slogin.cl.cam.ac.uk $KINIT" # run on SSH gateway

SSHFS="sshfs -o follow_symlinks -o uid=503 -o gid=20"

# CUCL

aod () {
    $KINIT
    $CLOGIN armyofdockerness.cl
}
aodfs () {
    $KINIT
    $SSHFS armyofdockerness.cl:/ ~/l/aod
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

moby-dev () {
    $SLOGIN -A moby-dev.packet
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
