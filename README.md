# Building

1. First make sure all the submodules are checked out.
   Run `git submodule init && git submodule update` in this directory
   and also in build-couchdb
1. cd build-couchdb
1. cat README.md # And make sure you installed the dependencies.
1. rake
1. cd ..
1. cp build-couchdb/build/lib/couchdb/erlang/lib/couch-*/ebin/*.beam ./ebin/

At this point you should be able to make a portable distribution like this:

    tar cvf fsck_couchdb.tar fsck_couchdb.escript ebin/

# Running

Untar to your database server.

    find . -type f -name '*.couch' -exec ./fsck_couchdb.escript {} \;
