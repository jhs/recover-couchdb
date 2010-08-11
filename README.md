# Building

1. First make sure all the submodules are checked out.
   Run `git submodule update --init` in this directory and also in
   build-couchdb
2. make

# Running

Untar to your database server.

    find . -type f -name '*.couch' -exec ./recover_couchdb {} \;

# Windows

You may need to run the `escript` command manually.

    escript ./recover_couchdb /path/to/my/db.couch
