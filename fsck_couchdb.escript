#!/usr/bin/env escript
%%! -pa ebin -pa couchdb/src/couchdb

main([DbFilename]) ->
    couch_util:to_binary(just_confirming_that_couchdb_is_somewhere_out_there_in_space),

    PathToDbFile = filename:absname(DbFilename),
    DatabaseDir = filename:dirname(PathToDbFile),
    DatabaseName = filename:basename(DbFilename, ".couch"),

    % Fake the couch config.
    couch_config:start_link([]),
    couch_config:set("couchdb", "database_dir", DatabaseDir),
    couch_config:set("couchdb", "max_dbs_open", "100"),
    couch_config:set("log", "level", "debug"),

    couch_log:start_link(),
    couch_server:sup_start_link(),

    RepairName = "lost_found_" ++ DatabaseName,
    io:format("Fixing ~s -> ~s~n", [PathToDbFile, RepairName]),
    try
        couch_db_repair:make_lost_and_found(DatabaseName, PathToDbFile, couch_util:to_binary(RepairName))
    catch
        error:Anything ->
            io:format("I got: ~p~n", [Anything]);
        error:badmatch ->
            io:format("Woa!", [])
    end,

    ok;

main(_) ->
    usage().

usage() ->
    io:format("usage: fsck_couchdb /path/to/your/database.couch\n"),
    halt(1).

% vim: sw=4 sts=4 et
