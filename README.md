# Building

1. First make sure all the submodules are checked out.
   Run `git submodule init && git submodule update` in this directory
   and also in build-couchdb
1. cd build-couchdb
1. cat README.md # And make sure you installed the dependencies.
1. rake
1. cd ..
1. ./fsck_couchdb.escript /path/to/my/db.couch
