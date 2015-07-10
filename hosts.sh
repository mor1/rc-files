#
# remote hosts
#

CUCL=slogin-serv.cl.cam.ac.uk
KRB5='cl-krenew -q --ensuretgt --maxout || kinit || /usr/kerberos/bin/kinit'
SSHFS="sshfs -o follow_symlinks"
cucl () {
    ssh -tx $CUCL $KRB5
    slogin -X $CUCL
}
cuclfs () {
    ssh -tx $CUCL $KRB5
    $SSHFS $CUCL:/home/rmm1002 ~/l/rmm1002
    $SSHFS $CUCL:/ ~/l/cucl
}
netos () {
    ssh -tx $CUCL $KRB5
    sshfs -o uid=503 -o gid=20 $CUCL:/usr/groups/netos ~/l/netos
}

hw2 () {
    slogin -X root@ubuntu.local
}
hw2fs() {
    sshfs ubuntu.local:/$1 ~/l/hw2
}

marian () {
    slogin -X marian.cs.nott.ac.uk
}
marianfs () {
    sshfs marian.cs.nott.ac.uk:/$1 ~/l/marian
}

mediapc () {
    slogin -X root@mediapc.home
}
mediapcfs() {
    sshfs root@mediapc.home:/$1 ~/l/mediapc
}

paws () {
    slogin paws-server
}
pawsfs () {
    sshfs paws-server:/$1 ~/l/paws
}

recoil () {
    slogin -X punk.recoil.org
}

severn () {
    slogin -X severn.cs.nott.ac.uk
}
severnfs () {
    sshfs severn.cs.nott.ac.uk:/$1 ~/l/severn
}

stmwww () {
    slogin -p 722 stthnorg@stthomasmorewollaton.org.uk
}
stmwwwfs() {
    sshfs -p 722 stthnorg@stthomasmorewollaton.org.uk: ~/l/stm-www
}

stratus () {
    slogin -X stratus.horizon.ac.uk
}
stratusfs () {
    sshfs -o uid=503 -o gid=20 stratus.cs.nott.ac.uk:/$1 ~/l/stratus
}

ucn () {
    slogin -X ucn-server
}

vagrantfs() {
    sshfs -p 2222 vagrant@localhost: ~/l/vagrant
}

xen () {
    slogin -X -p 2233 localhost
}
xenfs () {
    sshfs -p 2233 localhost: ~/l/xen
}
