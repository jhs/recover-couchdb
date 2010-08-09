#!/usr/bin/env escript
%%! -pa ebin -pa build-couchdb/build/lib/couchdb/erlang/lib/couch-1.0.1/ebin

main([DbFilename]) ->
    io:format("blah = ~p~n", [couch_util:to_binary(blah)]),
    io:format("If you see this message, then couchdb is available~n", []),

    try
        io:format("I should look at couch file: ~s\n", [DbFilename])
    catch
        _:_ ->
            usage()
    end;

main(_) ->
    usage().

usage() ->
    io:format("usage: fsck_couchdb /path/to/your/database.couch\n"),
    halt(1).

% vim: sw=4 sts=4 et
