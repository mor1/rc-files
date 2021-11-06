# Mort's `rc-files`

The collection of my configuration files, collated in a git repo since about
2009.

The `.emacs.d` is pulled out as a [separate
repository](https://github.com/mor1/rc-emacs)-- only the
[`README.md`](https://github.com/mor1/rc-emacs/blob/master/README.md) is
committed here.

Symlink farm management is now done using [`stow`](). Install from your distro's package (e.g., `sudo apt install stow`), or by hand via:


``` shell
git clone git@github.com:aspiers/stow.git
cd stow
autoreconf -iv
./configure --prefix=/auto/homes/rmm1002/.local
make && make install
```
