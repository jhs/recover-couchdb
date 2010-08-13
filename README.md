# Getting

You can clone this Git repository, but there are also pre-built binaries in the
[Download](recover-couchdb/archives/v0.5) section.

# Building

If you downloaded the `.zip` file from GitHub, this step is not required.

1. First make sure all the submodules are checked out by running
   Run `git submodule update --init` in this directory.
2. make

# Running

Copy recover_couchdb to your database server, possibly running
`chmod +x recover_couchb` if the copy process did not copy its execute
permission.

Next run the tool with all of your .couch files. For a normal source-code
install, that might look like this:

    ./recover_couchdb /usr/local/var/lib/couchdb/*.couch

# Windows

You may need to run the `escript` command manually.

    escript ./recover_couchdb /path/to/my/db.couch

# vim: tw=80
