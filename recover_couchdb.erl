-module(recover_couchdb).

-export([main/1]).

main(DbFiles) when is_list(DbFiles) ->
    couch_util:to_binary(just_confirming_that_couchdb_is_somewhere_out_there_in_space),

    % Fake the couch config.
    couch_config:start_link([]),
    couch_config:set("couchdb", "max_dbs_open", "100"),
    couch_config:set("log", "level", "debug"),
    couch_config:set("log", "file", "repair.log"),

    % This must be something or the servers won't start.
    [FirstDir | _Rest] = DbFiles,
    couch_config:set("couchdb", "database_dir", filename:dirname(FirstDir)),

    % Start required OTP servers.
    application:start(crypto),
    couch_log:start_link(),
    couch_rep_sup:start_link(),
    couch_task_status:start_link(),
    couch_server:sup_start_link(),
    gen_event:start_link({local, couch_db_update}),

    lists:foreach(fun(DbFile) ->
        PathToDbFile = filename:absname(DbFile),
        DatabaseDir = filename:dirname(PathToDbFile),
        DatabaseName = filename:basename(DbFile, ".couch"),
        RepairName = "lost+found/" ++ DatabaseName,

        io:format("Checking database: ~s~n", [DatabaseName]),
        io:format("Source file: ~s~n", [PathToDbFile]),
        io:format("Target file: ~s~n", [DatabaseDir ++ "/" ++ RepairName ++ ".couch"]),
        io:format("~n"),

        % Unfortunately neither of these work, database_dir crashes the program even.
        %couch_config:set("couchdb", "database_dir", DatabaseDir),
        %couch_config:set("log", "file", DatabaseName ++ "_recovery.log"),

        couch_db_repair:make_lost_and_found(DatabaseName, PathToDbFile, couch_util:to_binary(RepairName))
    end, DbFiles);

main(_) ->
    usage().

usage() ->
    io:format("usage: recover_couchdb /path/to/your/database.couch\n"),
    halt(1).

% vim: sw=4 sts=4 et
