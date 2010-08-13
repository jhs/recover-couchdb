% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_db_repair).

-compile(export_all).
-export([merge_to_file/2, make_lost_and_found/1,
         make_lost_and_found/3, find_nodes_quickly/1]).

-include("couch_db.hrl").

-define(CHUNK_SIZE, 1048576).
-define(SIZE_BLOCK, 4096).


merge_to_file(Db, TargetName) ->
    Options = [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}],
    case couch_db:open(TargetName, Options) of
    {ok, TargetDb0} ->
        ok;
    {not_found, no_db_file} ->
        {ok, TargetDb0} = couch_db:create(TargetName, Options)
    end,
    TargetDb = TargetDb0#db{fsync_options = [before_header]},

    {ok, _, {FinalDocs, Counter}} =
    couch_btree:foldl(Db#db.fulldocinfo_by_id_btree, fun(FDI, _, {Acc, I}) ->
        #doc_info{id=Id, revs = RevsInfo} = couch_doc:to_doc_info(FDI),
        LeafRevs = [Rev || #rev_info{rev=Rev} <- RevsInfo],
        {ok, Docs} = couch_db:open_doc_revs(Db, Id, LeafRevs, [latest]),
        {ok, {[Docs | Acc], I+1}}
    end, {[], 0}),
    FlatDocs = [Doc || {ok, Doc} <- lists:append(FinalDocs)],
    couch_db:update_docs(TargetDb, FlatDocs, [full_commit], replicated_changes),
    couch_db:close(TargetDb),
    Counter.

make_lost_and_found(DbName) ->
    TargetName = ?l2b(["lost+found/", DbName]),
    RootDir = couch_config:get("couchdb", "database_dir", "."),
    FullPath = filename:join([RootDir, "./" ++ DbName ++ ".couch"]),
    make_lost_and_found(DbName, FullPath, TargetName).

make_lost_and_found(DbName, FullPath, TargetName) ->
    {ok, Fd} = couch_file:open(FullPath, []),
    Options = [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}],
    {ok, Db} = couch_db:open(?l2b(DbName), Options),
    BtOptions = [
        {split, fun couch_db_updater:btree_by_id_split/1},
        {join, fun couch_db_updater:btree_by_id_join/2},
        {reduce, fun couch_db_updater:btree_by_id_reduce/3}
    ],
    put(dbname, DbName),
    {Nodes, ChildCount} = find_nodes_quickly(Fd),
    ?LOG_INFO("~p found ~p possible updates for ~s", [?MODULE, ChildCount,
        DbName]),
    lists:foldl(fun(Root, {Progress, RootCount}) ->
        {ok, Bt} = couch_btree:open({Root, 0}, Fd, BtOptions),
        try merge_to_file(Db#db{fulldocinfo_by_id_btree = Bt}, TargetName) of
        UpdateCount ->
                report_progress(Progress,UpdateCount,ChildCount,DbName,RootCount),
                {Progress + UpdateCount, RootCount+1}
        catch _:Reason ->
            ?LOG_ERROR("~p merge node at ~p ~p", [?MODULE, Root, Reason])
        end
    end, {0,0}, Nodes).

report_progress(Progress,UpdateCount,ChildCount,DbName,RootCount) when RootCount rem 100 == 0 ->
    ?LOG_INFO("~p processed ~p of ~p updates for ~s", [?MODULE,
                Progress + UpdateCount, ChildCount, DbName]);
report_progress(_,_,_,_,_) -> ok.

%% @doc returns a list of offsets in the file corresponding to locations of
%%      all kp and kv_nodes from the by_id tree
find_nodes_quickly(DbName) when is_list(DbName) ->
    RootDir = couch_config:get("couchdb", "database_dir", "."),
    FullPath = filename:join([RootDir, "./" ++ DbName ++ ".couch"]),
    {ok, Fd} = couch_file:open(FullPath, []),
    put(dbname, DbName),
    try find_nodes_quickly(Fd) after couch_file:close(Fd) end;
find_nodes_quickly(Fd) ->
    {ok, EOF} = couch_file:bytes(Fd),
    read_file(Fd, EOF, {[], 0}).

read_file(Fd, LastPos, Acc) ->
    ChunkSize = erlang:min(?CHUNK_SIZE, LastPos),
    Pos = LastPos - ChunkSize,
    ?LOG_INFO("~p for ~s - scanning ~p bytes at ~p", [?MODULE, get(dbname),
        ChunkSize, Pos]),
    {ok, Data, _} = gen_server:call(Fd, {pread, Pos, ChunkSize}),
    {Positions, Count} = read_data(Fd, Data, 0, Pos, Acc),
    if Pos == 0 ->
        {lists:reverse(Positions), Count};
    true ->
        read_file(Fd, Pos, {Positions, Count})
    end.

read_data(Fd, Data, Pos, Offset, Acc0) when Pos < byte_size(Data) ->
    FullOffset = Pos + Offset,
    % look for serialized terms that start with {kv_node,
    <<Pattern:12/binary, _/binary>> = term_to_binary({kv_node, nil}),
    Match = case Data of
    <<_:Pos/binary, Pattern:12/binary, _/binary>> ->
        % the ideal case, a full pattern match
        true;
    <<_:Pos/binary, SplitTerm:13/binary, _/binary>> when
            (FullOffset rem ?SIZE_BLOCK) > (?SIZE_BLOCK - 12) ->
        % check if the term is split across the block boundary
        N = ?SIZE_BLOCK - (FullOffset rem ?SIZE_BLOCK),
        M = 12 - N,
        case SplitTerm of <<Head:N/binary, 0, Tail:M/binary>> ->
            <<Head/binary, Tail/binary>> =:= Pattern;
        _Else ->
            % next block is a header, this can't be a kv_node
            false
        end;
    _ ->
        false
    end,
    Acc = if Match -> node_acc(Fd, FullOffset - 4, Acc0, true); true -> Acc0 end,
    read_data(Fd, Data, Pos+1, Offset, Acc);
read_data(_Fd, _Data, _Pos, _Offset, AccOut) ->
    AccOut.

node_acc(Fd, Pos, {Positions, ChildCount} = Acc, Retry) when Pos >= 0 ->
    case couch_file:pread_term(Fd, Pos) of
    {ok, {_, [{<<"_local/",_/binary>>,_}|_]}} ->
        Acc;
    {ok, {kv_node, [{<<_/binary>>,_}|_] = Children}} ->
        {[Pos | Positions], ChildCount + length(Children)};
    {ok, _} ->
        Acc;
    _Error ->
        if Retry, (Pos > 0) -> node_acc(Fd, Pos-1, Acc, false); true -> Acc end
    end;
node_acc(_, _, Acc, _) ->
    Acc.
