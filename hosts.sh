#
# remote hosts
#

SLOGIN="slogin -X"

CUCL=slogin.cl.cam.ac.uk
KRB5='cl-krenew -q --ensuretgt --maxout || kinit || /usr/kerberos/bin/kinit'
SSHFS="sshfs -o follow_symlinks -o uid=503 -o gid=20"

aod () {
    $SLOGIN armyofdockerness.cl.cam.ac.uk
}

cucl () {
    ssh -tx $CUCL $KRB5
    $SLOGIN $CUCL
}
cuclfs () {
    ssh -tx $CUCL $KRB5
    $SSHFS $CUCL:/home/rmm1002 ~/l/rmm1002
    $SSHFS $CUCL:/ ~/l/cucl
}
netos () {
    ssh -tx $CUCL $KRB5
    $SSHFS $CUCL:/usr/groups/netos ~/l/netos
}

marian () {
    $SLOGIN marian.cs.nott.ac.uk
}
marianfs () {
    sshfs marian.cs.nott.ac.uk:/$1 ~/l/marian
}

mediapc () {
    $SLOGIN root@mediapc.home
}
mediapcfs() {
    sshfs root@mediapc.home:/$1 ~/l/mediapc
}

monk () {
    $SLOGIN monk.recoil.org
}

office () {
    ssh -tx $CUCL $KRB5
    $SLOGIN daugleddau.cl
}

paws () {
    slogin paws-server
}
pawsfs () {
    sshfs paws-server:/$1 ~/l/paws
}

punk () {
    $SLOGIN punk.recoil.org
}

severn () {
    $SLOGIN severn.cs.nott.ac.uk
}
severnfs () {
    sshfs severn.cs.nott.ac.uk:/$1 ~/l/severn
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

stratus () {
    $SLOGIN stratus.horizon.ac.uk
}
stratusfs () {
    sshfs -o uid=503 -o gid=20 stratus.cs.nott.ac.uk:/$1 ~/l/stratus
}

ucn () {
    $SLOGIN ucn-server
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
