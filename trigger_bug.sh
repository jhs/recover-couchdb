#!/bin/bash
#
# Trigger the missing root bug.

server="http://localhost:5984"
db="bad_db_$$"

run_curl () {
  data=''
  if [ "$3" ]; then
    if [ "$3" != '"true"' ]; then
      # Just a simple doc.
      data="--data-binary '{\"message\":\"$2\"}'"
    else
      # Ugly workaround to met me set _config
      data="--data-binary '$3'"
    fi
  fi

  cmd="curl -X $1 -H 'Content-Type: application/json' ${server}$2 $data"
  echo "$cmd"
  bash -c "$cmd"
}

put  () { run_curl PUT    "$@"; }
del  () { run_curl DELETE "$@"; }
get  () { run_curl GET    "$@"; }
post () { run_curl POST   "$@"; }

put /_config/couchdb/delayed_commits \"true\"
get /_config/couchdb/delayed_commits

del /$db
put /$db

put /$db/put1 'I am a normal document, 1'
put /$db/put2 'I am a normal document, 2'

echo "Waiting for data to sync"
sleep 3

put /$db/con1 'I will cause conflict, because I am a rascal, 1'
put /$db/con1 'I will not be stored because I conflict with con1'

for a in `seq 1 10`; do
  put /$db/where$a "Where did where$a go?"
done

get /$db/_all_docs
post /_restart
echo "Waiting for server to restart"
sleep 3
get /$db/_all_docs
