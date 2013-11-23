#
# remote hosts
#

stratus () {
  slogin -X stratus.horizon.ac.uk
}

marian () {
  slogin -X marian.cs.nott.ac.uk
}

severn () {
  slogin -X severn.cs.nott.ac.uk
}

roach () {
  slogin -X roach.cs.nott.ac.uk
}

cucl () {
  slogin -X rmm1002@slogin-serv.cl.cam.ac.uk
}

devbox () {
  slogin -X ppsrm.nottingham.ac.uk
}

hw2 () {
  slogin -X root@ubuntu.local
}

stmwww() {
  slogin -p 722 -X stthnorg@stthomasmorewollaton.org.uk
}
  
mediapc () {
  slogin -X root@mediapc.home
}

#
# remote fs
#

marianfs () {
  sshfs marian.cs.nott.ac.uk:/$1 ~/l/marian
}

severnfs () {
  sshfs severn.cs.nott.ac.uk:/$1 ~/l/severn
}

stratusfs () {
  sshfs stratus.cs.nott.ac.uk:/$1 ~/l/stratus
}

hw2fs() {
  sshfs ubuntu.local:/$1 ~/l/hw2
}

stmwwwfs() {
  sshfs -p 722 stthnorg@stthomasmorewollaton.org.uk: ~/l/stm-www
}

vagrantfs() {
  sshfs -p 2222 vagrant@localhost: ~/l/vagrant
}

mediapcfs() {
  sshfs root@mediapc.home:/$1 ~/l/mediapc
}

cuclfs () {
  sshfs rmm1002@slogin-serv.cl.cam.ac.uk:$1 ~/l/cucl
}
