# Building

1. First make sure all the submodules are checked out by running
   Run `git submodule update --init` in this directory.
2. make

# Running

Copy recover_couchdb to your database server, possibly running
`chmod +x recover_couchb` if the copy process did not copy its execute
permission.

Next run the tool with all of your .couch files.

    find . -type f -name '*.couch' -exec ./recover_couchdb {} \;

# Windows

You may need to run the `escript` command manually.

    escript ./recover_couchdb /path/to/my/db.couch

# vim: tw=80
