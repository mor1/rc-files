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
  
#
# remote fs
#

marianfs () {
  sshfs marian.cs.nott.ac.uk:/$1 ~/m/marian
}

severnfs () {
  sshfs severn.cs.nott.ac.uk:/$1 ~/m/severn
}

stratusfs () {
  sshfs stratus.cs.nott.ac.uk:/$1 ~/m/stratus
}

hw2fs() {
  sshfs ubuntu.local:/$1 ~/m/hw2
}
