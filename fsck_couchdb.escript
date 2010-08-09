#!/usr/bin/env escript
%%! -pa ebin -pa couchdb/src/couchdb

main([DbFilename]) ->
    couch_util:to_binary(just_confirming_that_couchdb_is_somewhere_out_there_in_space),

    Fd = try
        couch_file:open(DbFilename)
    catch
        _:_ ->
            usage()
    end,

    io:format("Accessing ~s~n", [DbFilename]),
    {ok, OldHeader} = couch_file:read_header(Fd),

    ok;

main(_) ->
    usage().

usage() ->
    io:format("usage: fsck_couchdb /path/to/your/database.couch\n"),
    halt(1).

% vim: sw=4 sts=4 et
