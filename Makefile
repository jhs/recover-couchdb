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

clean:
	rm -rf ebin
	rm -f ebin.zip
	rm -f recover_couchdb

# vim: sw=8 sts=8 noet
