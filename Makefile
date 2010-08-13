# Builds the recover_couchdb tool

erls = $(wildcard couchdb/src/couchdb/*.erl)
beams = $(patsubst couchdb/src/couchdb/%.erl,ebin/%.beam,$(erls))

recover_couchdb: ebin.zip
	echo '#!/usr/bin/env escript' > $@
	echo '%%! -smp enable -escript main recover_couchdb' >> $@
	cat ebin.zip >> $@
	chmod +x $@

ebin:
	mkdir -p ebin

ebin/recover_couchdb.beam: recover_couchdb.erl ebin
	erlc -o ebin recover_couchdb.erl

$(beams): ebin/%.beam: couchdb/src/couchdb/%.erl
	erlc -o ebin $<

ebin.zip: ebin/recover_couchdb.beam $(beams)
	cd ebin && zip -9 ../ebin.zip *.beam

clean_ebin:
	rm -rf ebin
	rm -f ebin.zip

clean: clean_ebin
	rm -f recover_couchdb

tag: clean
	read -p "This is a pretty destructive operation. Press ^C if you are frightened:" foo
	rm -rf couchdb
	git submodule update
	rm -f .gitmodules
	rm -fr couchtest
	git rm couchtest
	mv couchdb couchdb_tmp
	git rm couchdb
	mv couchdb_tmp couchdb
	rm -rf couchdb/.git
	git add couchdb
	make recover_couchdb
	make clean_ebin
	git add --force recover_couchdb
	echo "I think you are ready to commit and tag."

# vim: sw=8 sts=8 noet
