tools/sh.scripts
================

Unix Shell Configuration and Shell-Script-Based Tools

---

 

This is a selection of my shell-scripts [mainly "bash"-based] and
`*rc` files.  Here's a quick overview:


* `env/`

  Contains my `tcshrc` and my (quite extensive and multifile) `bashrc`
  files.  The `bashrc` files are designed to work in a variety of Unix
  environments.  Provided to help you with your own shell configs.

* `droid/`

  I'm a big fan of the terminal, even on Android-based devices!  This
  directory contains scripts, aliases, and functions that help me get
  by on an Android terminal app running Busybox.  Feel free to grab
  anything you find useful.

* `file-purger.sh`

  A cleanup-script that looks for backup-files [e.g. files ending in
  `~` or `#`] and relocates them to a "purge directory."
  If you run it under a symlink named `safe-rm.sh`, it will behave
  like an `rm`, but will move the files to the "purge directory"
  instead of deleting them.

  Designed to work with both the non-GNU- and GNU-version of the
  common Unix cmdline tools.

* `pathtools.sh`

  Meant to be sourced.  Contains functions for manipulating your
  `$PATH` envvar (like, say, removing duplicate paths).

* `git-helpers.sh`

  Also meant to be sourced.  Contains functions that (A) bundle together
  sequences of git commands that I'm likely to forget; (B) save me
  some typing by filling in some defaults that I'll always use; (C)
  perform complex tasks with one function.

  You may find them useful.  You might not.

* `mount-fusefs.sh`

  Now, this one is an interesting tool.  Using a `fstab`-like file and
  the FUSE filesystem utilities, it will mount a remote directory over
  either FTP or SSH.  What distinguishes it from using `sshfs` or
  `curlftpfs` directly is that you can specify only the preconfigured
  mountpoint, or even an alias, and the script will use your
  "fusefstab" to take care of the rest.  You can even configure in
  behavior akin to `mount -a` --- very useful if you always mount the
  same remote directories every time you log in!

  Run it with the `--help` option for its full usage/manpage.
